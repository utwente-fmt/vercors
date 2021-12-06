package vct.col.ast.temporaryimplpackage.lang

import vct.col.ast.{JavaClass, Type}

trait JavaClassImpl { this: JavaClass =>
  override def supports: Seq[Type] = ext +: imp
}