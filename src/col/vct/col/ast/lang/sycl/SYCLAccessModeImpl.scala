package vct.col.ast.lang.sycl

import vct.col.ast.SYCLAccessMode

trait SYCLAccessModeImpl[G] { this: SYCLAccessMode[G] =>
  val name: String
}