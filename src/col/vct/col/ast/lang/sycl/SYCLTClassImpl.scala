package vct.col.ast.lang.sycl

import vct.col.ast.SYCLTClass

trait SYCLTClassImpl[G] {
  this: SYCLTClass[G] =>
  val namespacePath: String
}
