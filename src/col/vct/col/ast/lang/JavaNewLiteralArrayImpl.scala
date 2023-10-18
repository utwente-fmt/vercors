package vct.col.ast.lang

import hre.util.FuncTools
import vct.col.ast.{JavaNewLiteralArray, TArray, Type}
import vct.col.print.{Ctx, Doc, Precedence, Text}

trait JavaNewLiteralArrayImpl[G] {
  this: JavaNewLiteralArray[G] =>
  override lazy val t: Type[G] = FuncTools
    .repeat[Type[G]](TArray(_), dims, baseType)

  override def precedence: Int = Precedence.POSTFIX
  override def layout(implicit ctx: Ctx): Doc =
    Text("new") <+> baseType <> "[]".repeat(dims) <> initializer
}
