package vct.col.ast.declaration.global

import vct.col.ast.util.Declarator
import vct.col.ast.{AxiomaticDataType, Declaration}

trait AxiomaticDataTypeImpl[G] extends Declarator[G] { this: AxiomaticDataType[G] =>
  override def declarations: Seq[Declaration[G]] = decls ++ typeArgs
}