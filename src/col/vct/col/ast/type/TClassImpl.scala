package vct.col.ast.`type`

import vct.col.ast.{TClass, Class}

trait TClassImpl[G] { this: TClass[G] =>
  def transSupportArrows: Seq[(Class[G], Class[G])] = cls.decl.transSupportArrows
}