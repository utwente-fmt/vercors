package vct.col.ast.lang.java

import vct.col.ast.JavaNamedType
import vct.col.print._
import vct.col.ast.ops.JavaNamedTypeOps

trait JavaNamedTypeImpl[G] extends JavaNamedTypeOps[G] { this: JavaNamedType[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Doc.fold(names.map {
      case (name, None) => Text(name)
      case (name, Some(typeArgs)) => Text(name) <> "<" <> Doc.fold(typeArgs)(_ <> "," <+> _) <> ">"
    })(_ <> "." <> _)
}