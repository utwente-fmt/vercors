package vct.col.ast.lang.c

import vct.col.ast.CEnumMemberDeclarator
import vct.col.ast.ops.{
  CEnumMemberDeclaratorFamilyOps,
  CEnumMemberDeclaratorOps,
}
import vct.col.print.{Ctx, Doc, Text}

trait CEnumMemberDeclaratorImpl[G]
    extends CEnumMemberDeclaratorOps[G] with CEnumMemberDeclaratorFamilyOps[G] {
  this: CEnumMemberDeclarator[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    if (value.isEmpty)
      Text(name)
    else
      Text(name) <+> "=" <+> value.get <> ";"
}
