package vct.col.ast.expr.bip

import vct.col.ast.{BipGuardInvocation, TBool, Type}
import vct.col.print.{Ctx, Doc, Group, Precedence, Text}
import vct.col.ast.ops.BipGuardInvocationOps

trait BipGuardInvocationImpl[G] extends BipGuardInvocationOps[G] { this: BipGuardInvocation[G] =>
  override def t: Type[G] = TBool()

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc =
    assoc(obj) <> "." <> ctx.name(guard) <> "(/* ... */)"
}
