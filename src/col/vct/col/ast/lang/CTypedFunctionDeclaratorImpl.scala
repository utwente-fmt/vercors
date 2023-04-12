package vct.col.ast.lang

import vct.col.ast.CTypedFunctionDeclarator
import vct.col.print.{Ctx, Doc, Text, Group}

trait CTypedFunctionDeclaratorImpl[G] { this: CTypedFunctionDeclarator[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(inner.show <> "(" <> Doc.args(params ++ (if(varargs) Seq(Text("...")) else Nil)) <> ")")
}