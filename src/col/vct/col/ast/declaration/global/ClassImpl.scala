package vct.col.ast.declaration.global

import vct.col.ast.Class
import vct.col.ast.util.Declarator
import vct.col.print._
import vct.col.util.AstBuildHelpers.tt
import vct.result.VerificationError.Unreachable
import vct.col.ast.ops.ClassOps

trait ClassImpl[G] extends Declarator[G] with ClassOps[G] { this: Class[G] =>
  protected def transSupportArrows(seen: Set[Class[G]]): Seq[(Class[G], Class[G])] =
    if(seen.contains(this)) Nil
    else supports.map(other => (this, other.decl)) ++
      supports.flatMap(other => other.decl.transSupportArrows(Set(this) ++ seen))

  def transSupportArrows: Seq[(Class[G], Class[G])] = transSupportArrows(Set.empty)

  def layoutLockInvariant(implicit ctx: Ctx): Doc =
    Text("lock_invariant") <+> intrinsicLockInvariant <+/> Empty

  override def layout(implicit ctx: Ctx): Doc =
    (if(intrinsicLockInvariant == tt[G]) Empty else Doc.spec(Show.lazily(layoutLockInvariant(_)))) <>
      Group(
        Text("class") <+> ctx.name(this) <>
        (if(supports.isEmpty) Empty else Text(" implements") <+> Doc.args(supports.map(ctx.name).map(Text))) <+>
        "{"
      ) <>>
      Doc.stack(declarations) <+/>
    "}"
}