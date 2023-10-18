package vct.col.ast.`type`

import vct.col.ast.TString
import vct.col.print.{Ctx, Doc, Text}

trait TStringImpl[G] {
  this: TString[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Text(ctx.syntax match {
      case Ctx.PVL => "string"
      case Ctx.Java => "String"
      case _ => "string"
    })
}
