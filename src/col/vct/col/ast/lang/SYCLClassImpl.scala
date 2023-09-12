package vct.col.ast.lang

import vct.col.ast.SYCLClass
import vct.col.print.{Ctx, Doc, Group, Text}

trait SYCLClassImpl[G] { this: SYCLClass[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(Text("SYCLClass") <> "(" <> Text(name) <>
        (if (genericArg.isDefined) (Text("<") <> Text(genericArg.get.toString) <> Text(">")) else Text("")) <>
    ")")
}