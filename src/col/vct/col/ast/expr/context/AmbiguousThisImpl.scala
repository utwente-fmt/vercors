package vct.col.ast.expr.context

import vct.col.ast._
import vct.col.err.ContextSensitiveNodeNotResolved
import vct.col.print.{Ctx, Doc, Precedence, Text}
import vct.col.resolve.ctx._
import vct.col.ast.ops.AmbiguousThisOps

trait AmbiguousThisImpl[G] extends AmbiguousThisOps[G] { this: AmbiguousThis[G] =>
  override lazy val t: Type[G] =
    ref.getOrElse(
      throw ContextSensitiveNodeNotResolved(this,
        "'this' encountered, but the surrounding class is not resolved.")
    ) match {
      case RefJavaClass(decl) => JavaTClass(decl.ref, Nil)
      case RefClass(decl) => TClass(decl.ref, decl.typeArgs.map((v: Variable[G]) => TVar(v.ref[Variable[G]])))
      case RefModel(decl) => TModel(decl.ref)
      case RefPVLSeqProg(decl) => TPVLSeqProg(decl.ref)
      case RefSeqProg(decl) => TSeqProg(decl.ref)
    }

  override def precedence: Int = Precedence.ATOMIC
  override def layout(implicit ctx: Ctx): Doc = Text("this")
}