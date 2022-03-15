package vct.col.ast.temporaryimplpackage.declaration.global

import vct.col.ast.{Class, PinnedDecl}
import vct.col.ast.temporaryimplpackage.util.Declarator
import vct.result.VerificationResult.Unreachable

trait ClassImpl[G] extends Declarator[G] { this: Class[G] =>
  protected def transSupportArrows(seen: Set[Class[G]]): Seq[(Class[G], Class[G])] =
    if(seen.contains(this)) throw Unreachable("Yes, you got me, cyclical inheritance is not supported!")
    else supports.map(other => (this, other.decl)) ++
      supports.flatMap(other => other.decl.transSupportArrows(Set(this) ++ seen))

  def transSupportArrows: Seq[(Class[G], Class[G])] = transSupportArrows(Set.empty)

  def isPin(p: PinnedDecl[G]) = pin.contains(p)
}