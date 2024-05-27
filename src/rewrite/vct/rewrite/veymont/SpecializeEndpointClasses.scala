package vct.rewrite.veymont

import com.typesafe.scalalogging.LazyLogging
import hre.util.ScopedStack
import vct.col.ast.util.Declarator
import vct.col.ast.{AbstractRewriter, ApplicableContract, Assert, Assign, Block, BooleanValue, Branch, ChorGuard, ChorRun, ChorStatement, Choreography, Class, ClassDeclaration, Communicate, CommunicateX, Constructor, ConstructorInvocation, Declaration, Deref, Endpoint, EndpointName, Eval, Expr, GlobalDeclaration, InstanceField, InstanceMethod, JavaClass, JavaConstructor, JavaInvocation, JavaLocal, JavaMethod, JavaNamedType, JavaParam, JavaPublic, JavaTClass, Local, LocalDecl, Loop, MethodInvocation, NewObject, Node, Procedure, Program, RunMethod, Scope, Statement, TClass, TVeyMontChannel, TVoid, ThisChoreography, ThisObject, Type, UnitAccountedPredicate, Variable, VeyMontAssignExpression, WritePerm}
import vct.col.origin.{Name, Origin, PanicBlame, SourceName}
import vct.col.ref.Ref
import vct.col.resolve.ctx.RefJavaMethod
import vct.col.rewrite.adt.{ImportADT, ImportADTImporter}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, RewriterBuilderArg, Rewritten}
import vct.col.util.SuccessionMap
import vct.result.VerificationError.{Unreachable, UserError}
import vct.col.util.AstBuildHelpers._
import vct.result.VerificationError

object SpecializeEndpointClasses extends RewriterBuilder {
  override def key: String = "specializeEndpointClasses"
  override def desc: String = "Replaces classes of endpoints with new classes that contain an instance of the original class, allowing for further transformation of the endpoint class. Also generates auxiliary annotations for endpoints, such as distinctness."
}

case class SpecializeEndpointClasses[Pre <: Generation]() extends Rewriter[Pre] with LazyLogging with VeymontContext[Pre] {

  val implFieldOfEndpoint = SuccessionMap[Endpoint[Pre], InstanceField[Post]]()
  val classOfEndpoint = SuccessionMap[Endpoint[Pre], Class[Post]]()

  override def dispatch(expr: Expr[Pre]): Expr[Post] = expr match {
    case name @ EndpointName(Ref(endpoint)) =>
      implicit val o = name.o
      Deref[Post](name.rewriteDefault(), implFieldOfEndpoint.ref(endpoint))(PanicBlame("Should be safe"))
    case _ => expr.rewriteDefault()
  }

  override def dispatch(decl: Declaration[Pre]): Unit = decl match {
    case chor: Choreography[Pre] => currentChoreography.having(chor) {
      super.dispatch(chor)
    }

    case endpoint: Endpoint[Pre] =>
      implicit val o = endpoint.o

      val implField = new InstanceField[Post](dispatch(endpoint.t), Seq())(o.where(name = "impl"))
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
                (Deref[Post](`this`, implField.ref)(PanicBlame("Deref cannot fail")) === implArg.get))
          ),
          body = Some(assignField[Post](ThisObject(classOfEndpoint.ref(endpoint)), implField.ref, implArg.get, PanicBlame("Cannot fail"))),
          outArgs = Seq(), typeArgs = Seq(),
        )(PanicBlame("Postcondition cannot fail"))
      }

      val wrapperClass = new Class[Post](
        typeArgs = Seq(), supports = Seq(), intrinsicLockInvariant = tt,
        decls = Seq(
          implField,
          constructor
        )
      )(endpoint.o.where(indirect = Name.names(Name("Endpoint"), endpoint.o.getPreferredNameOrElse())))
      classOfEndpoint(endpoint) = wrapperClass
      globalDeclarations.declare(wrapperClass)

      allScopes.anySucceed(endpoint, endpoint.rewrite[Post](
        cls = wrapperClass.ref,
        typeArgs = Seq(),
        constructor = constructor.ref,
        args = Seq(constructorInvocation[Post](
          ref = succ(endpoint.constructor.decl),
          classTypeArgs = endpoint.typeArgs.map(dispatch),
          args = endpoint.args.map(dispatch),
          blame = PanicBlame("Not implemented")
        )),
        blame = PanicBlame("Unreachable")
      ))
    case _ => super.dispatch(decl)
  }
}
