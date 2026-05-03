package vct.rewrite.veymont.generation

import com.typesafe.scalalogging.LazyLogging
import hre.util.ScopedStack
import vct.col.ast.{And, ApplicableContract, Assert, Assign, Assume, Block, BooleanValue, Branch, ByReferenceClass, ChorRun, ChorStatement, Choreography, Class, ClassDeclaration, CommTargetEndpoint, CommTargetIndex, CommTargetSingle, CommunicateTarget, Constructor, Declaration, Deref, Endpoint, EndpointExpr, EndpointStatement, Eval, Exhale, Expr, FieldLocation, Fork, IdleToken, Inhale, InstanceField, InstanceMethod, IterationContract, Join, Local, Location, Loop, LoopContract, LoopInvariant, Node, Or, Perm, Procedure, Program, RangeBinder, RunMethod, Scope, Statement, TSeq, TVoid, ThisObject, Type, Value, Variable, WritePerm}
import vct.col.origin.{DiagnosticOrigin, Name, Origin, PanicBlame, PostBlameSplit}
import vct.col.ref.Ref
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.AstBuildHelpers._
import vct.col.util.AstMatchHelpers.EndpointName
import vct.col.util.SuccessionMap
import vct.result.Message
import vct.result.VerificationError.SystemError
import vct.rewrite.veymont.VeymontContext
import vct.rewrite.veymont.generation.GenerateImplementation.CannotProjectStatement

import scala.collection.mutable

object GenerateImplementation extends RewriterBuilder {
  override def key: String = "generateImplementation"

  override def desc: String =
    "Generate classes for VeyMont threads in parallel program"

  case class CannotProjectStatement(target: CommTargetSingle[_], s: Statement[_])
      extends SystemError {
    override def text: String = {
      Message.messagesInContext(
        (
          s.o,
          s"The following statement was not transformed into a supported statement...",
        ),
        (target.ref.decl.o, "... while projecting for this endpoint."),
      )
    }
  }
}

case class GenerateImplementation[Pre <: Generation]()
    extends Rewriter[Pre] with VeymontContext[Pre] with LazyLogging {
  outer =>

  val runSucc: mutable.LinkedHashMap[Choreography[Pre], Procedure[Post]] =
    mutable.LinkedHashMap()
  private val givenClassSucc: SuccessionMap[Type[Pre], Class[Post]] =
    SuccessionMap()
  private val givenClassConstrSucc: SuccessionMap[Type[Pre], Procedure[Pre]] =
    SuccessionMap()
  val endpointLocals: SuccessionMap[Endpoint[Pre], Variable[Post]] =
    SuccessionMap()

  // For each endpoint and input variable, there is a unique instance field (on the class of the endpoint)
  val endpointParamFields =
    SuccessionMap[(Endpoint[Pre], Variable[Pre]), InstanceField[Post]]()
  // For each endpoint and another endpoint, there is a unique instance field (on the class of the endpoint)
  // with a reference to the second endpoint
  val endpointPeerFields =
    SuccessionMap[(Endpoint[Pre], Endpoint[Pre]), InstanceField[Post]]()

  val currentThis = ScopedStack[ThisObject[Post]]()

  override def dispatch(p: Program[Pre]): Program[Post] = {
    mappings.program = p
    super.dispatch(p)
  }

  override def dispatch(decl: Declaration[Pre]): Unit = {
    decl match {
      case p: Procedure[Pre] => super.dispatch(p)

      case cons: Constructor[Pre] if isEndpointClass(classOf(cons)) =>
        implicit val o = cons.o
        cons.rewrite(
          contract = cons.contract.rewrite(ensures =
            (IdleToken[Post](ThisObject(succ(classOf(cons)))) &*
              endpointContext(endpointOf(classOf(cons)), perm = WritePerm()))
              .accounted &* dispatch(cons.contract.ensures)
          ),
          blame = PostBlameSplit
            .left(PanicBlame("Automatically generated permissions"), cons.blame),
        ).succeed(cons)

      case cls: Class[Pre] if isEndpointClass(cls) =>
        implicit val o = DiagnosticOrigin
        val endpoint = endpointOf(cls)
        val chor = choreographyOf(endpoint)
        val commTarget = endpoint.range match {
          case Some(RangeBinder(_, _, _)) => ??? // Something like: CommTargetIndex(endpoint.ref, some field probably?)
          case None => CommTargetEndpoint[Pre](endpoint.ref[Endpoint[Pre]])
        }
        currentThis.having(ThisObject[Post](succ(cls))(cls.o)) {
          cls.rewrite(decls =
            classDeclarations.collect {
              cls.decls.foreach(dispatch)
              generateRunMethod(chor, commTarget)
              generateParamFields(chor, endpoint)
              generatePeerFields(chor, endpoint)
            }._1
          )
        }.succeed(cls)

      case cls: Class[Pre] => super.dispatch(cls)
      case chor: Choreography[Pre] =>
        currentChoreography.having(chor) {
          implicit val o = chor.o
          chor.drop()

          val initEndpoints = chor.endpoints.map { endpoint =>
            endpoint.drop()

            val newClassT = dispatch(endpoint.cls.decl.classType(Seq()))
            if (endpoint.isSingle) {
              endpointLocals(endpoint) = new Variable(newClassT)(endpoint.o)
            } else {
              endpointLocals(endpoint) = new Variable(TSeq(newClassT))(endpoint.o)
            }

            assignLocal[Post](
              endpointLocals(endpoint).get,
              dispatch(endpoint.init),
            )
          }

          // Initialize the fields on each endpoint class, representing the parameters of the choreography, and other endpoints
          val auxFieldAssigns = chor.endpoints.flatMap { endpoint =>
            chor.params.map { param =>
              assignField[Post](
                endpointLocals(endpoint).get,
                endpointParamFields.ref((endpoint, param)),
                Local(succ(param)),
                blame = PanicBlame("Should be safe"),
              )
            } ++ chor.endpoints.map { peer =>
              assignField[Post](
                endpointLocals(endpoint).get,
                endpointPeerFields.ref((endpoint, peer)),
                endpointLocals(peer).get,
                blame = PanicBlame("Should be safe"),
              )
            }
          }

          val forkJoins =
            chor.endpoints.map { endpoint =>
              Fork[Post](endpointLocals(endpoint).get)(PanicBlame(""))
            } ++ chor.endpoints.map { endpoint =>
              Join[Post](endpointLocals(endpoint).get)(PanicBlame(""))
            }

          val mainBody = Scope(
            chor.endpoints.map(endpointLocals(_)),
            Block(
              initEndpoints ++
                Seq(chor.preRun.map(dispatch).getOrElse(Block(Seq()))) ++
                auxFieldAssigns ++ forkJoins
            )(chor.o),
          )

          globalDeclarations.declare(
            new Procedure(
              contract = chor.contract.rewriteDefault(),
              returnType = TVoid[Post](),
              args = variables.dispatch(chor.params),
              body = Some(mainBody),
              outArgs = Seq(),
              typeArgs = Seq(),
            )(PanicBlame("TODO: Procedure"))(chor.o)
          )
        }
      case other => super.dispatch(other)
    }
  }

  def generateRunMethod(
      chor: Choreography[Pre],
      target: CommTargetSingle[Pre],
  ): Unit = {
    val run = chor.run
    implicit val o = run.o
    currentChoreography.having(chor) {
      currentTarget.having(target) {
        classDeclarations.declare(
          new RunMethod(
            body = Some(projectStmt(run.body)(target)),
            contract = projectContract(run.contract)(target),
          )(PanicBlame(""))
        )
      }
    }
  }

  def generateParamFields(
      chor: Choreography[Pre],
      endpoint: Endpoint[Pre],
  ): Unit =
    chor.params.foreach { param =>
      val f =
        new InstanceField(dispatch(param.t), Seq())(param.o.where(indirect =
          Name.names(
            chor.o.getPreferredNameOrElse(),
            Name("p"),
            param.o.getPreferredNameOrElse(),
          )
        ))
      classDeclarations.declare(f)
      endpointParamFields((endpoint, param)) = f
    }

  def generatePeerFields(
      chor: Choreography[Pre],
      endpoint: Endpoint[Pre],
  ): Unit =
    chor.endpoints.foreach { peer =>
      val f =
        new InstanceField(dispatch(peer.singleType), Seq())(
          endpoint.o
            .where(indirect = Name.names(peer.o.getPreferredNameOrElse()))
        )
      classDeclarations.declare(f)
      endpointPeerFields((endpoint, peer)) = f
    }

  // All permissions that we want to be in scope within the run method of an endpoint:
  // the peer fields and param fields
  def endpointContext(
      endpoint: Endpoint[Pre],
      perm: Expr[Post] = null,
      value: Boolean = false,
  )(implicit o: Origin): Expr[Post] = {
    assert(value ^ (perm != null))

    def makePerm(loc: Location[Post]): Expr[Post] =
      if (perm != null)
        Perm(loc, perm)
      else
        Value(loc)

    val cls = endpoint.cls.decl
    val `this` = ThisObject[Post](succ(cls))
    val chor = choreographyOf(endpoint)
    val peerPerms = foldStar(chor.endpoints.map { peer =>
      val ref = endpointPeerFields
        .ref[Post, InstanceField[Post]]((endpoint, peer))
      makePerm(FieldLocation(`this`, ref))
    })

    val paramPerms = foldStar(chor.params.map { param =>
      val ref = endpointParamFields
        .ref[Post, InstanceField[Post]]((endpoint, param))
      makePerm(FieldLocation(`this`, ref))
    })

    peerPerms &* paramPerms
  }

  override def dispatch(statement: Statement[Pre]): Statement[Post] = {
    if (currentTarget.nonEmpty)
      projectStmt(statement)(currentTarget.top)
    else
      super.dispatch(statement)
  }

  override def dispatch(expr: Expr[Pre]): Expr[Post] =
    expr match {
      case InChor(_, EndpointName(Ref(endpoint))) =>
        Local[Post](endpointLocals.ref(endpoint))(expr.o)
      case InEndpoint(_, target, EndpointName(Ref(peer))) =>
        implicit val o = expr.o
        Deref[Post](currentThis.top, endpointPeerFields.ref((target.ref.decl, peer)))(
          PanicBlame("Shouldn't happen")
        )
      case InEndpoint(_, target, Local(Ref(v)))
          if currentChoreography.nonEmpty && currentTarget.nonEmpty &&
            isChoreographyParam(v) =>
        implicit val o = expr.o
        Deref[Post](currentThis.top, endpointParamFields.ref((target.ref.decl, v)))(
          PanicBlame("Shouldn't happen")
        )
      case InEndpoint(_, target, expr) => projectExpr(expr)(target)
      case _ => expr.rewriteDefault()
    }

  def projectStmt(
      statement: Statement[Pre]
  )(implicit target: CommTargetSingle[Pre]): Statement[Post] =
    statement match {
      case EndpointStatement(None, statement) =>
        statement match {
          case _ =>
            throw new Exception(
              "Encountered ChorStatement without endpoint context"
            )
        }
      case EndpointStatement(Some(target), inner)
          if target == target =>
        inner match {
          case assign: Assign[Pre] => assign.rewriteDefault()
          case eval: Eval[Pre] => eval.rewriteDefault()
        }
      // Ignore statements that do not match the current endpoint
      case EndpointStatement(_, _) => Block(Seq())(statement.o)
      // Specialize composite statements to the current endpoint
      case c @ ChorStatement(branch: Branch[Pre])
          if c.explicitEndpoints.contains(target) =>
        // TODO (RR): For supporting target = CommTargetIndex, need to check here if endpoint index is in range in various places. Just checking the explicitEndpoints list is not sound. An if is included for each endpoint in a total endpoint range, but it should only be executed for those actually in the range. So I think the explicitEndpoints check actually has to become semantic, and wrap the actually projected if. Kind of like this: if (currentEndpoint in range) { if (actual projected if with regular projected expressions) }. This might actually be a bug in the paper as well...?
        implicit val o = branch.o
        Branch[Post](
          Seq((projectExpr(branch.cond), projectStmt(branch.yes))) ++
            branch.no.map(no => Seq((tt[Post], projectStmt(no))))
              .getOrElse(Seq())
        )
      case c @ ChorStatement(l: Loop[Pre])
          if c.explicitEndpoints.contains(target) =>
        // TODO (RR): For supporting target = CommTargetIndex, need to check here if endpoint index is in range. See comment above.
        implicit val o = l.o
        loop(
          cond = projectExpr(l.cond),
          body = projectStmt(l.body),
          contract = projectContract(l.contract),
        )
      // Ignore loops, branches that the current endpoint doesn't participate in
      case c @ ChorStatement(_: Loop[Pre] | _: Branch[Pre]) => Block(Seq())(c.o)
      // Project assert-like statements as you'd expect
      case ChorStatement(assert: Assert[Pre]) =>
        assert.rewrite(res = projectExpr(assert.res))
      case ChorStatement(assume: Assume[Pre]) =>
        assume.rewrite(assn = projectExpr(assume.assn))
      case ChorStatement(inhale: Inhale[Pre]) =>
        inhale.rewrite(res = projectExpr(inhale.res))
      case ChorStatement(exhale: Exhale[Pre]) =>
        exhale.rewrite(res = projectExpr(exhale.res))
      // Rewrite blocks transparently
      case block: Block[Pre] => block.rewriteDefault()
      // Plain assigns were warned about in LangVeyMontToCol.
      // We just keep them in the program here for debugging purposes.
      case assign: Assign[Pre] => assign.rewriteDefault()
      // Don't let any missed cases slip through
      case s => throw CannotProjectStatement(target, s)
    }

  def projectContract(
      contract: LoopContract[Pre]
  )(implicit target: CommTargetSingle[Pre]): LoopContract[Post] = {
    implicit val o = contract.o
    contract match {
      case inv: LoopInvariant[Pre] =>
        inv.rewrite(invariant =
          endpointContext(target.ref.decl, value = true) &* projectExpr(inv.invariant)
        )
      case it: IterationContract[Pre] =>
        it.rewrite(
          requires =
            endpointContext(target.ref.decl, value = true) &* projectExpr(it.requires),
          ensures =
            endpointContext(target.ref.decl, value = true) &* projectExpr(it.ensures),
        )
    }
  }

  def projectContract(
      contract: ApplicableContract[Pre]
  )(implicit target: CommTargetSingle[Pre]): ApplicableContract[Post] = {
    implicit val o = contract.o
    contract.rewrite(
      requires =
        endpointContext(target.ref.decl, value = true).accounted &*
          mapPredicate(contract.requires, projectExpr),
      ensures =
        endpointContext(target.ref.decl, value = true).accounted &*
          mapPredicate(contract.ensures, projectExpr),
      contextEverywhere = projectExpr(contract.contextEverywhere),
    )
  }

  def projectExpr(
      expr: Expr[Pre]
  )(implicit target: CommTargetSingle[Pre]): Expr[Post] =
    expr match {
      case EndpointExpr(CommTargetEndpoint(Ref(ep)), Seq(), expr)
          if ep == target.ref.decl =>
        projectExpr(expr)
      case EndpointExpr(CommTargetEndpoint(Ref(ep)), Seq(), expr)
        if ep != target.ref.decl => BooleanValue(true)(expr.o)
      case EndpointExpr(ct, bindings, expr) =>
        ??? // TODO (RR): Handle encoding bindings, probably as forall?
      // Define transparent projections for basic operators
      case and: And[Pre] =>
        and.rewrite(projectExpr(and.left), projectExpr(and.right))
      case or: Or[Pre] =>
        or.rewrite(projectExpr(or.left), projectExpr(or.right))
      // Actually just doing rewriteDefault is kind of wrong and buggy. But it covers most default & correct cases so
      // I'll leave it for now
      case _ => expr.rewriteDefault()
    }
}
