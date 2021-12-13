package vct.col.ast.temporaryimplpackage.expr.context

import vct.col.ast._
import vct.col.err.ContextSensitiveNodeNotResolved
import vct.col.resolve.{RefClass, RefJavaClass, RefModel}

trait AmbiguousThisImpl[G] { this: AmbiguousThis[G] =>
  override def t: Type[G] =
    ref.getOrElse(
      throw ContextSensitiveNodeNotResolved(this,
        "'this' encountered, but the surrounding class is not resolved.")
    ) match {
      case RefJavaClass(decl) => JavaTClass(decl.ref, Nil)
      case RefClass(decl) => TClass(decl.ref)
      case RefModel(decl) => TModel(decl.ref)
    }
}