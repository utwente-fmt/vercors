package vct.col.ast.expr.context

import vct.col.ast._
import vct.col.err.ContextSensitiveNodeNotResolved
import vct.col.resolve.ctx._

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