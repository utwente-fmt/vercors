package vct.col.ast.lang

import vct.col.ast.SYCLClassDefName
import vct.col.print.{Ctx, Doc, Group, Text}

trait SYCLClassDefNameImpl[G] { this: SYCLClassDefName[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text(name) <>
        (if (genericArg.isDefined) (Text("<") <> Text(genericArg.get.toString) <> Text(">")) else Text("")))
}