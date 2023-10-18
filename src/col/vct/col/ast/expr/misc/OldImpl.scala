package vct.col.ast.expr.misc

import vct.col.ast.{Old, Type}
import vct.col.print.{Ctx, Doc, Precedence, Text}

trait OldImpl[G] {
  this: Old[G] =>
  override def t: Type[G] = expr.t

  override def precedence: Int = Precedence.ATOMIC
  override def layout(implicit ctx: Ctx): Doc =
    (ctx.syntax, at) match {
      case (Ctx.Silver, None) => Text("old(") <> expr <> ")"
      case (Ctx.Silver, Some(at)) =>
        Text("old[") <> ctx.name(at) <> "](" <> expr <> ")"
      case (_, None) => Text("\\old(") <> expr <> ")"
      case (_, Some(at)) =>
        Text("\\old[") <> ctx.name(at) <> "](" <> expr <> ")"
    }
}
