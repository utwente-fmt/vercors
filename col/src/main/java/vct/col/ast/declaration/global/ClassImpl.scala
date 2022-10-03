package vct.col.ast.declaration.global

import vct.col.ast.{Class, PinnedDecl}
import vct.col.ast.util.Declarator
import vct.result.VerificationError.Unreachable

trait ClassImpl[G] extends Declarator[G] { this: Class[G] =>
  protected def transSupportArrows(seen: Set[Class[G]]): Seq[(Class[G], Class[G])] =
    if(seen.contains(this)) Nil
    else supports.map(other => (this, other.decl)) ++
      supports.flatMap(other => other.decl.transSupportArrows(Set(this) ++ seen))

  def transSupportArrows: Seq[(Class[G], Class[G])] = transSupportArrows(Set.empty)

  def isPin(p: PinnedDecl[G]) = pin.contains(p)
}