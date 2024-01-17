package vct.col.ast.lang.c

import vct.col.ast.CTypedFunctionDeclarator
import vct.col.print.{Ctx, Doc, Text, Group}
import vct.col.ast.ops.CTypedFunctionDeclaratorOps

trait CTypedFunctionDeclaratorImpl[G] extends CTypedFunctionDeclaratorOps[G] { this: CTypedFunctionDeclarator[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(inner.show <> "(" <> Doc.args(params ++ (if(varargs) Seq(Text("...")) else Nil)) <> ")")
}