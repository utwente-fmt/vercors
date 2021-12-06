package vct.col.ast.temporaryimplpackage.declaration.global

import vct.col.ast.temporaryimplpackage.util.Declarator
import vct.col.ast.{AxiomaticDataType, Declaration}

trait AxiomaticDataTypeImpl extends Declarator { this: AxiomaticDataType =>
  override def declarations: Seq[Declaration] = decls ++ typeArgs
}