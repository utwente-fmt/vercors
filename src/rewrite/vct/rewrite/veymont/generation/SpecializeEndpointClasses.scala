package vct.rewrite.veymont.generation

import com.typesafe.scalalogging.LazyLogging
import vct.col.ast.{
  Choreography,
  Class,
  Constructor,
  Declaration,
  Deref,
  Endpoint,
  EndpointName,
  Expr,
  InstanceField,
  ThisObject,
  UnitAccountedPredicate,
  Variable,
  WritePerm,
}
import vct.col.origin.{Name, PanicBlame}
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

  val implFieldOfEndpoint = SuccessionMap[Endpoint[Pre], InstanceField[Post]]()
  val classOfEndpoint = SuccessionMap[Endpoint[Pre], Class[Post]]()

  override def dispatch(expr: Expr[Pre]): Expr[Post] =
    expr match {
      case name @ EndpointName(Ref(endpoint)) =>
        implicit val o = name.o
        Deref[Post](name.rewriteDefault(), implFieldOfEndpoint.ref(endpoint))(
          PanicBlame("Should be safe")
        )
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
        implFieldOfEndpoint(endpoint) = implField

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
      case _ => super.dispatch(decl)
    }
}
