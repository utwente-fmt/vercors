package vct.col.newrewrite

import hre.util.ScopedStack
import vct.col.ast._
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.AstBuildHelpers._
import vct.col.ast.RewriteHelpers._
import vct.col.origin.{AbstractApplicable, Origin, PanicBlame}
import vct.col.ref.Ref

case object ConstantifyFinalFields extends RewriterBuilder

case class ConstantifyFinalFields[Pre <: Generation]() extends Rewriter[Pre] {
  val currentClass: ScopedStack[Class[Pre]] = ScopedStack()

  def isFinal(field: InstanceField[Pre]): Boolean =
    field.flags.collectFirst { case _: Final[Pre] => () }.isDefined

  override def dispatch(decl: Declaration[Pre]): Unit = decl match {
    case cls: Class[Pre] =>
      currentClass.having(cls) { rewriteDefault(cls) }
    case field: InstanceField[Pre] =>
      implicit val o: Origin = field.o
      if(isFinal(field)) {
        function[Post](
          blame = AbstractApplicable,
          returnType = dispatch(field.t),
          args = Seq(new Variable[Post](TClass(succ(currentClass.top)))),
        ).succeedDefault(field)
      } else {
        rewriteDefault(field)
      }
    case other => rewriteDefault(other)
  }

  override def dispatch(e: Expr[Pre]): Expr[Post] = e match {
    case Deref(obj, Ref(field)) =>
      implicit val o: Origin = e.o
      if(isFinal(field)) FunctionInvocation[Post](succ(field), Seq(dispatch(obj)), Nil, Nil, Nil)(PanicBlame("requires nothing"))
      else rewriteDefault(e)
    case other => rewriteDefault(other)
  }

  override def dispatch(stat: Statement[Pre]): Statement[Post] = stat match {
    case Assign(Deref(obj, Ref(field)), value) =>
      implicit val o: Origin = stat.o
      if(isFinal(field)) Inhale(FunctionInvocation[Post](succ(field), Seq(dispatch(obj)), Nil, Nil, Nil)(PanicBlame("requires nothing")) === dispatch(value))
      else rewriteDefault(stat)
    case other => rewriteDefault(other)
  }
}
