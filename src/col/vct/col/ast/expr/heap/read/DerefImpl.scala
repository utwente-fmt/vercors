package vct.col.ast.expr.heap.read

import vct.col.ast.expr.ExprImpl
import vct.col.ast.{
  Deref,
  EndpointName,
  Expr,
  FieldLocation,
  TClass,
  Type,
  Value,
}
import vct.col.check.{Check, CheckContext, CheckError, SeqProgReceivingEndpoint}
import vct.col.print.{Ctx, Doc, Group, Precedence}
import vct.col.ref.Ref
import vct.col.ast.ops.DerefOps

trait DerefImpl[G] extends ExprImpl[G] with DerefOps[G] {
  this: Deref[G] =>
  override def t: Type[G] =
    obj.t.asClass.map(_.instantiate(ref.decl.t)).getOrElse(ref.decl.t)

  override def check(context: CheckContext[G]): Seq[CheckError] =
    Check.inOrder(
      super.check(context),
      obj.t.asClass.get.cls.decl.checkDefines(ref.decl, this),
    )

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc =
    assoc(obj) <> "." <> ctx.name(ref)

  def value: Value[G] = Value(FieldLocation(obj, ref))
}
