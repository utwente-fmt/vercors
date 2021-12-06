package vct.col.ast.temporaryimplpackage.declaration.global

import vct.col.ast.Class
import vct.col.ast.temporaryimplpackage.util.Declarator
import vct.result.VerificationResult.Unreachable

trait ClassImpl extends Declarator { this: Class =>
  protected def transSupportArrows(seen: Set[Class]): Seq[(Class, Class)] =
    if(seen.contains(this)) throw Unreachable("Yes, you got me, cyclical inheritance is not supported!")
    else supports.map(other => (this, other.decl)) ++
      supports.flatMap(other => other.decl.transSupportArrows(Set(this) ++ seen))

  def transSupportArrows: Seq[(Class, Class)] = transSupportArrows(Set.empty)
}