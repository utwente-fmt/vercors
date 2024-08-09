package vct.rewrite.veymont.generation

import com.typesafe.scalalogging.LazyLogging
import vct.col.ast.{
  ChorRun,
  Choreography,
  Class,
  Communicate,
  Constructor,
  Declaration,
  Deref,
  Endpoint,
  EndpointExpr,
  EndpointName,
  Expr,
  FieldLocation,
  InstanceField,
  IterationContract,
  LoopContract,
  LoopInvariant,
  Program,
  Receiver,
  Sender,
  ThisObject,
  UnitAccountedPredicate,
  Value,
  Variable,
  WritePerm,
}
import vct.col.origin.{Name, Origin, PanicBlame, PostBlameSplit}
import vct.col.ref.Ref
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.AstBuildHelpers._
import vct.col.util.SuccessionMap
import vct.rewrite.veymont.VeymontContext

object SpecializeEndpointClasses extends RewriterBuilder {
  override def key: String = "specializeEndpointClasses"
  override def desc: String =
    "Replaces classes of endpoints with new classes that contain an instance of the original class, allowing for further transformation of the endpoint class. Also generates auxiliary annotations for endpoints, such as distinctness."
}

case class SpecializeEndpointClasses[Pre <: Generation]()
    extends Rewriter[Pre] with LazyLogging with VeymontContext[Pre] {

  val implFields = SuccessionMap[Endpoint[Pre], InstanceField[Post]]()
  val classOfEndpoint = SuccessionMap[Endpoint[Pre], Class[Post]]()

  override def dispatch(p: Program[Pre]): Program[Post] = {
    mappings.program = p
    super.dispatch(p)
  }

  def readImplField(obj: Expr[Post], endpoint: Endpoint[Pre])(
      implicit o: Origin
  ): Expr[Post] = {
    Deref[Post](obj, implFields.ref(endpoint))(PanicBlame(
      "Permissions for impl should be automatically generated"
    ))
  }

  override def dispatch(expr: Expr[Pre]): Expr[Post] =
    expr match {
      case EndpointName(Ref(endpoint)) =>
        readImplField(expr.rewriteDefault(), endpoint)(expr.o)
      case Sender(Ref(comm)) =>
        readImplField(expr.rewriteDefault(), comm.sender.get.decl)(expr.o)
      case Receiver(Ref(comm)) =>
        readImplField(expr.rewriteDefault(), comm.receiver.get.decl)(expr.o)
      case _ => expr.rewriteDefault()
    }

  override def dispatch(decl: Declaration[Pre]): Unit =
    decl match {
      case chor: Choreography[Pre] =>
        currentChoreography.having(chor) { super.dispatch(chor) }

      case endpoint: Endpoint[Pre] =>
        implicit val o = endpoint.o

        val implField =
          new InstanceField[Post](dispatch(endpoint.t), Seq())(
            o.where(name = "impl")
          )
        implFields(endpoint) = implField

        val constructor: Constructor[Post] = {
          val implArg = new Variable(dispatch(endpoint.t))
          val `this` = new ThisObject[Post](classOfEndpoint.ref(endpoint))
          new Constructor[Post](
            cls = classOfEndpoint.ref(endpoint),
            args = Seq(implArg),
            contract = contract[Post](
              blame = PanicBlame("TODO"),
              ensures = UnitAccountedPredicate(
                fieldPerm[Post](`this`, implField.ref, WritePerm()) &*
                  (Deref[Post](`this`, implField.ref)(PanicBlame(
                    "Deref cannot fail"
                  )) === implArg.get)
              ),
            ),
            body = Some(assignField[Post](
              ThisObject(classOfEndpoint.ref(endpoint)),
              implField.ref,
              implArg.get,
              PanicBlame("Cannot fail"),
            )),
            outArgs = Seq(),
            typeArgs = Seq(),
          )(PanicBlame("Postcondition cannot fail"))
        }

        val wrapperClass =
          new Class[Post](
            typeArgs = Seq(),
            supports = Seq(),
            intrinsicLockInvariant = tt,
            decls = Seq(implField, constructor),
          )(endpoint.o.where(indirect =
            Name.names(Name("Endpoint"), endpoint.o.getPreferredNameOrElse())
          ))
        classOfEndpoint(endpoint) = wrapperClass
        globalDeclarations.declare(wrapperClass)

        allScopes.anySucceed(
          endpoint,
          endpoint.rewrite[Post](
            cls = wrapperClass.ref,
            typeArgs = Seq(),
            init = constructorInvocation(
              ref = constructor.ref,
              args = Seq(dispatch(endpoint.init)),
              blame = PanicBlame("Should be safe"),
            ),
          ),
        )

      case comm: Communicate[Pre] =>
        val sender = comm.sender.get.decl
        val receiver = comm.receiver.get.decl
        val newComm: Ref[Post, Communicate[Post]] = succ(comm)
        implicit val o = comm.o
        comm.rewrite(invariant =
          value[Post](Sender(newComm), implFields.ref(sender)) &*
            value[Post](Receiver(newComm), implFields.ref(receiver)) &*
            dispatch(comm.invariant)
        ).succeed(comm)

      case _ => super.dispatch(decl)
    }

  override def dispatch(run: ChorRun[Pre]): ChorRun[Post] = {
    implicit val o = run.o
    run.rewrite(
      contract = run.contract.rewrite(
        requires =
          specializeContext(currentChoreography.top).accounted &*
            dispatch(run.contract.requires),
        ensures =
          specializeContext(currentChoreography.top).accounted &*
            dispatch(run.contract.ensures),
      ),
      blame = PostBlameSplit
        .left(PanicBlame("Automatically generated permissions"), run.blame),
    )
  }

  override def dispatch(contract: LoopContract[Pre]): LoopContract[Post] =
    contract match {
      case InChor(chor, inv: LoopInvariant[Pre]) =>
        implicit val o = contract.o
        inv.rewrite(invariant =
          specializeContext(chor) &* dispatch(inv.invariant)
        )
      case InChor(chor, inv: IterationContract[Pre]) =>
        implicit val o = contract.o
        inv.rewrite(
          requires = specializeContext(chor) &* dispatch(inv.requires),
          ensures = specializeContext(chor) &* dispatch(inv.ensures),
        )
      case _ => contract.rewriteDefault()
    }

  // Within a choreography, we need to propagate permission for the impl fields of all endpoint to all endpoints
  def specializeContext(
      chor: Choreography[Pre]
  )(implicit o: Origin): Expr[Post] = {
    foldStar(chor.endpoints.flatMap { endpoint =>
      chor.endpoints.map { peer =>
        EndpointExpr[Post](
          succ(endpoint),
          Value(
            FieldLocation(EndpointName[Post](succ(peer)), implFields.ref(peer))
          ),
        )
      }
    })
  }
}
