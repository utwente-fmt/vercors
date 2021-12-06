package vct.col.newrewrite

import hre.util.ScopedStack
import vct.col.ast._
import vct.col.rewrite.Rewriter
import vct.col.util.AstBuildHelpers._
import vct.col.ast.RewriteHelpers._
import vct.col.origin.{AbstractApplicable, Origin, PanicBlame}
import vct.col.ref.Ref

case class ConstantifyFinalFields() extends Rewriter {
  val currentClass: ScopedStack[Class] = ScopedStack()

  def isFinal(field: InstanceField): Boolean =
    field.flags.collectFirst { case _: Final => () }.isDefined

  override def dispatch(decl: Declaration): Unit = decl match {
    case cls: Class =>
      currentClass.having(cls) { rewriteDefault(cls) }
    case field: InstanceField =>
      implicit val o: Origin = field.o
      if(isFinal(field)) {
        function(
          blame = AbstractApplicable,
          returnType = field.t,
          args = Seq(new Variable(TClass(succ(currentClass.top)))),
        ).succeedDefault(this, field)
      } else {
        rewriteDefault(field)
      }
    case other => rewriteDefault(other)
  }

  override def dispatch(e: Expr): Expr = e match {
    case Deref(obj, Ref(field)) =>
      implicit val o: Origin = e.o
      if(isFinal(field)) FunctionInvocation(succ(field), Seq(dispatch(obj)), Nil)(PanicBlame("requires nothing"))
      else rewriteDefault(e)
    case other => rewriteDefault(other)
  }

  override def dispatch(stat: Statement): Statement = stat match {
    case Assign(Deref(obj, Ref(field)), value) =>
      implicit val o: Origin = stat.o
      if(isFinal(field)) Inhale(FunctionInvocation(succ(field), Seq(dispatch(obj)), Nil)(PanicBlame("requires nothing")) === dispatch(value))
      else rewriteDefault(stat)
    case other => rewriteDefault(other)
  }
}
