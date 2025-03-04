package vct.rewrite.veymont.verification

import com.typesafe.scalalogging.LazyLogging
import hre.util.ScopedStack
import vct.col.ast.{
  Assign,
  Assume,
  Block,
  ChorExpr,
  ChorRun,
  ChorStatement,
  Choreography,
  CommTargetEndpoint,
  CommTargetIndex,
  CommTargetRange,
  Communicate,
  CtExpr,
  Declaration,
  Endpoint,
  EndpointExpr,
  EndpointFamilyExpr,
  EndpointStatement,
  Eval,
  Expr,
  InstanceMethod,
  IterVariable,
  Local,
  Message,
  MethodInvocation,
  Perm,
  Procedure,
  RangeBinder,
  RangedFor,
  ReadPerm,
  Receiver,
  Scope,
  Sender,
  SeqSubscript,
  Statement,
  TVoid,
  ThisChoreography,
  Value,
  Variable,
}
import vct.col.origin._
import vct.col.ref.Ref
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.AstBuildHelpers._
import vct.col.util.{Scopes, SuccessionMap}
import vct.result.VerificationError.Unreachable

import scala.collection.{mutable => mut}

object EncodeChoreography extends RewriterBuilder {
  override def key: String = "encodeChoreography"
  override def desc: String = "Encodes the semantics of a VeyMont choreography."

  object SignalsAlwaysEmpty extends PanicBlame("signals always empty")

  case class CallableFailureToContractedFailure(blame: Blame[ContractedFailure])
      extends Blame[CallableFailure] {
    override def blame(error: CallableFailure): Unit =
      error match {
        case failure: ContractedFailure => blame.blame(failure)
        case SignalsFailed(failure, node) => SignalsAlwaysEmpty.blame(error)
        case ExceptionNotInSignals(node) => SignalsAlwaysEmpty.blame(error)
      }
  }

  case class AssignFailedToSeqAssignFailure(assign: EndpointStatement[_])
      extends Blame[AssignFailed] {
    override def blame(error: AssignFailed): Unit =
      assign.blame.blame(SeqAssignInsufficientPermission(assign))
  }

  case class AssertFailedToParticipantsNotDistinct(comm: Communicate[_])
      extends Blame[AssertFailed] {
    override def blame(error: AssertFailed): Unit =
      comm.blame.blame(ParticipantsNotDistinct(comm))
  }

  case class InvocationFailureToChorRunFailure(run: ChorRun[_])
      extends Blame[InvocationFailure] {
    override def blame(error: InvocationFailure): Unit =
      error match {
        case PreconditionFailed(path, fail, node) =>
          run.blame.blame(ChorRunPreconditionFailed(Some(path), fail, run))
        case ContextEverywhereFailedInPre(fail, node) =>
          run.blame.blame(ChorRunContextEverywhereFailedInPre(fail, run))
      }
  }
}

case class EncodeChoreography[Pre <: Generation]()
    extends Rewriter[Pre] with LazyLogging {
  import EncodeChoreography._

  val currentProg: ScopedStack[Choreography[Pre]] = ScopedStack()
  val currentRun: ScopedStack[ChorRun[Pre]] = ScopedStack()
  val currentCommunicate: ScopedStack[Communicate[Pre]] = ScopedStack()

  val sender: ScopedStack[Expr[Post]] = ScopedStack()
  val receiver: ScopedStack[Expr[Post]] = ScopedStack()
  val message: ScopedStack[Expr[Post]] = ScopedStack()

  val eps: Scopes[Pre, Post, Endpoint[Pre], Variable[Post]] = new Scopes()

  val methodSucc: SuccessionMap[InstanceMethod[Pre], Procedure[Post]] =
    SuccessionMap()
  val msgSucc: SuccessionMap[Communicate[Pre], Variable[Post]] = SuccessionMap()

  override def dispatch(decl: Declaration[Pre]): Unit =
    decl match {
      case chor: Choreography[Pre] => rewriteChoreography(chor)

      case _ => super.dispatch(decl)
    }

  def rewriteChoreography(chor: Choreography[Pre]): Procedure[Post] =
    currentProg.having(chor) {
      eps.scope {
        // First generate a procedure that implements the run method
        val runProc = rewriteRun(chor)

        // And also process all auxiliary instance methods
        rewriteChorMethods(chor)

        // Then generate a procedure that initializes all the endpoints and calls the run procedure
        // First set up the successor variables that will be encoding the choreography argument and endpoints
        implicit val o = chor.o
        chor.endpoints.foreach(_.drop())
        declareChorEndpointVars(chor)

        // Maintain successor for choreography argument variables manually, as two contexts are maintained
        // The main procedure context and run procedure contex
        chor.params.foreach(_.drop())
        declareChorParamVars(chor)

        // TODO (RR): I think this init we can maybe reuse for the endpoint projection routines as well, as it is also what I did in the summation case study
        // For each endpoint, make a local variable and initialize it using the constructor referenced in the endpoint
        val endpointsInit = chor.endpoints.map {
          case endpoint if endpoint.isSingle =>
            Assign(Local[Post](eps(endpoint).ref), dispatch(endpoint.init))(
              AssignLocalOk
            )
          case endpoint if endpoint.isFamily =>
            val RangeBinder(i, low, high) = endpoint.range.get
            RangedFor(
              IterVariable(
                variables.dispatch(i),
                dispatch(low),
                dispatch(high),
              ),
              loopInvariant(blame = PanicBlame("???")),
              Block(Seq(Assume[Post](ff))),
            )
        }

        val preRun = chor.preRun.map(dispatch).toSeq

        // Invoke the run procedure with the seq_program arguments, as well as all the endpoints
        val invokeRun = Eval(procedureInvocation[Post](
          ref = runProc.ref,
          args =
            chor.params.map(arg => Local[Post](succ(arg))) ++
              chor.endpoints
                .map(endpoint => Local[Post](eps.freeze.succ(endpoint))),
          blame = InvocationFailureToChorRunFailure(chor.run),
        ))

        // Scope the endpoint vars and combine initialization and run method invocation
        val body = Scope(
          chor.endpoints.map(eps(_)),
          Block((endpointsInit ++ preRun) :+ invokeRun),
        )

        chor.drop()
        globalDeclarations.declare(
          new Procedure(
            returnType = TVoid(),
            outArgs = Seq(),
            typeArgs = Seq(),
            args = chor.params.map(variables(_)),
            contract = dispatch(chor.contract),
            body = Some(body),
          )(CallableFailureToContractedFailure(chor.blame))
        )
      }
    }

  def rewriteChorMethods(chor: Choreography[Pre]): Seq[Procedure[Post]] =
    chor.decls.collect { case m: InstanceMethod[Pre] =>
      rewriteChorMethod(chor, m)
    }

  def chorContextVariables(chor: Choreography[Pre]): Seq[Variable[Post]] =
    chor.params.map(arg => variables(arg)) ++
      chor.endpoints.map(endpoint => eps(endpoint))

  def rewriteChorMethod(
      chor: Choreography[Pre],
      method: InstanceMethod[Pre],
  ): Procedure[Post] = {
    assert(method.args.isEmpty) // TODO: Pretty error
    eps.scope {
      variables.scope {
        declareChorEndpointVars(chor)
        declareChorParamVars(chor)

        methodSucc(method) = globalDeclarations.declare(
          new Procedure(
            args = chorContextVariables(chor),
            body = method.body.map(dispatch),
            outArgs = Nil,
            typeArgs = Nil,
            returnType = dispatch(method.returnType),
            contract = dispatch(method.contract),
          )(method.blame)(method.o)
        )
        methodSucc(method)
      }
    }
  }

  def declareChorEndpointVars(chor: Choreography[Pre]): Unit =
    for (ep <- chor.endpoints) {
      val t =
        if (ep.isSingle)
          ep.singleType
        else
          ep.rangeType
      eps.succeedOnly(ep, new Variable(dispatch(t))(ep.o))
    }

  def declareChorParamVars(chor: Choreography[Pre]): Unit =
    for (arg <- chor.params) {
      variables.succeedOnly(arg, new Variable(dispatch(arg.t))(arg.o))
    }

  def rewriteRun(chor: Choreography[Pre]): Procedure[Post] =
    eps.scope {
      variables.scope {
        val run = chor.run
        implicit val o: Origin = run.o.where(name =
          currentProg.top.o.getPreferredNameOrElse().snake + "_run"
        )

        currentRun.having(run) {
          declareChorEndpointVars(chor)
          declareChorParamVars(chor)

          globalDeclarations.declare(
            new Procedure(
              args = chorContextVariables(chor),
              contract = dispatch(run.contract),
              body = Some(dispatch(run.body)),
              outArgs = Seq(),
              typeArgs = Seq(),
              returnType = TVoid(),
            )(CallableFailureToContractedFailure(run.blame))
          )
        }
      }
    }

  override def dispatch(stat: Statement[Pre]): Statement[Post] =
    stat match {
      case ChorStatement(inner) => dispatch(inner)
      case EndpointStatement(_, _) =>
        throw Unreachable(
          "Encoding endpoint statements should be handled by EncodePermissionStratification"
        )
      case stat => stat.rewriteDefault()
    }

  override def dispatch(expr: Expr[Pre]): Expr[Post] =
    expr match {
      case CtExpr(target) =>
        implicit val o = expr.o
        target match {
          case CommTargetEndpoint(Ref(endpoint)) => Local(eps(endpoint).ref)
          case CommTargetIndex(Ref(endpoint), i) =>
            SeqSubscript(Local[Post](eps(endpoint).ref), dispatch(i))(
              PanicBlame("Should forward to CtExpr here...")
            )
          case CommTargetRange(Ref(endpoint), range) =>
            // TODO: This case will be gone when we remove CtExpr.
            ???
        }
      case EndpointFamilyExpr(Ref(ep)) => Local[Post](eps(ep).ref)(expr.o)
      case inv @ MethodInvocation(
            ThisChoreography(_),
            Ref(method),
            _,
            _,
            _,
            _,
            _,
          ) =>
        implicit val o = inv.o
        val prog = currentProg.top
        procedureInvocation(
          ref = methodSucc.ref(method),
          args =
            prog.params
              .map(arg => Local[Post](variables.freeze.succ(arg))(arg.o)) ++
              prog.endpoints
                .map(endpoint => Local[Post](eps.freeze.succ(endpoint))(inv.o)),
          blame = inv.blame,
        )
      case Sender(Ref(comm)) => sender.top
      case Receiver(Ref(comm)) => receiver.top
      case Message(Ref(comm)) => message.top
      case _: ChorExpr[_] | _: EndpointExpr[_] =>
        throw Unreachable(
          "Encoding of ChorExpr and EndpointExpr should happen in EncodePermissionStratification"
        )
      case Perm(loc, ReadPerm()) =>
        // For now we manually translate the readperms away because we accidentally introduce them as well
        Value(dispatch(loc))(expr.o)
      case _ => expr.rewriteDefault()
    }
}
