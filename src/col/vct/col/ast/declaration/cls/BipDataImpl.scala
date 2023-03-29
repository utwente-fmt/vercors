package vct.col.ast.declaration.cls

import vct.col.ast.{BipData, Type}

trait BipDataImpl[G] { this: BipData[G] =>
  def t: Type[G]
}
