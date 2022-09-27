package vct.col.ast.lang

import vct.col.ast.CGpgpuKernelSpecifier
import vct.col.origin.{Blame, KernelFailure}

trait CGpgpuKernelSpecifierImpl[G] { this: CGpgpuKernelSpecifier[G] =>
  def blame: Blame[KernelFailure]
}