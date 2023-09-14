package vct.col.ast.lang

import vct.col.ast.CPPTypedFunctionDeclarator
import vct.col.print.{Ctx, Doc, Group, Text}

trait CPPTypedFunctionDeclaratorImpl[G] { this: CPPTypedFunctionDeclarator[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(inner.show <> "(" <> Doc.args(params ++ (if(varargs) Seq(Text("...")) else Nil)) <> ")")
}