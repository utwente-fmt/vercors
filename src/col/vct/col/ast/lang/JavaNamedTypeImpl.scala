package vct.col.ast.lang

import vct.col.ast.JavaNamedType
import vct.col.print._

trait JavaNamedTypeImpl[G] {
  this: JavaNamedType[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Doc.fold(names.map {
      case (name, None) => Text(name)
      case (name, Some(typeArgs)) =>
        Text(name) <> "<" <> Doc.fold(typeArgs)(_ <> "," <+> _) <> ">"
    })(_ <> "." <> _)
}
