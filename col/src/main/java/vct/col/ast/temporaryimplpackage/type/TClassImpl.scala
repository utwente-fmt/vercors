package vct.col.ast.temporaryimplpackage.`type`

import vct.col.ast.{TClass, Class}

trait TClassImpl { this: TClass =>
  def transSupportArrows: Seq[(Class, Class)] = cls.decl.transSupportArrows
}