package vct.col.ast.lang.cpp

import vct.col.ast.CPPTypedFunctionDeclarator
import vct.col.print.{Ctx, Doc, Group, Text}
import vct.col.ast.ops.CPPTypedFunctionDeclaratorOps

trait CPPTypedFunctionDeclaratorImpl[G] extends CPPTypedFunctionDeclaratorOps[G] { this: CPPTypedFunctionDeclarator[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(inner.show <> "(" <> Doc.args(params ++ (if(varargs) Seq(Text("...")) else Nil)) <> ")")
}