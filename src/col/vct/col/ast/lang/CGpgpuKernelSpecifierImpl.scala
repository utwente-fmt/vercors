package vct.col.ast.lang

import vct.col.ast.{Blame1, CGpgpuKernelSpecifier}
import vct.col.origin.{Blame, KernelFailure}

trait CGpgpuKernelSpecifierImpl[G] { this: CGpgpuKernelSpecifier[G] =>
  def blame: Blame1[G]
}