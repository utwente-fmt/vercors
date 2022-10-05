package vct.col.ast.lang

import vct.col.ast.JavaEnumConstant

trait JavaEnumConstantImpl[G] { this: JavaEnumConstant[G] =>

  val isStatic: Boolean = true

}
