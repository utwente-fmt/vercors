package vct.col.ast.temporaryimplpackage.declaration.global

import vct.col.ast.Class
import vct.col.ast.temporaryimplpackage.util.Declarator
import vct.result.VerificationResult.Unreachable

trait ClassImpl[G] extends Declarator[G] { this: Class[G] =>
  protected def transSupportArrows(seen: Set[Class[G]]): Seq[(Class[G], Class[G])] =
    if(seen.contains(this)) Nil
    else supports.map(other => (this, other.decl)) ++
      supports.flatMap(other => other.decl.transSupportArrows(Set(this) ++ seen))

  def transSupportArrows: Seq[(Class[G], Class[G])] = transSupportArrows(Set.empty)
}