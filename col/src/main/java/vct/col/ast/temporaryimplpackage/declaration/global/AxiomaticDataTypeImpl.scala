package vct.col.ast.temporaryimplpackage.declaration.global

import vct.col.ast.temporaryimplpackage.util.Declarator
import vct.col.ast.{ADTAxiom, ADTFunction, AxiomaticDataType, Declaration}

trait AxiomaticDataTypeImpl[G] extends Declarator[G] { this: AxiomaticDataType[G] =>
  override def declarations: Seq[Declaration[G]] = decls ++ typeArgs

  lazy val functions: Seq[ADTFunction[G]] = decls.collect({ case f: ADTFunction[G] => f})
  lazy val axioms: Seq[ADTAxiom[G]] = decls.collect({ case a: ADTAxiom[G] => a})
}