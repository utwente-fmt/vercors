package vct.col.ast.util

import hre.ast.Origin
import hre.lang.System._
import vct.col.ast.generic.ASTNode

trait VisitorHelper {
  def getOrigin(): Origin

  def handle_standard[T](fun: () => T) = try fun() catch {
    case t: Throwable => handle_throwable(t)
  }

  /**
   * This function is used in many AST classes to handle/print exceptions when
   * executing a visitor pattern over the AST.
   */
  def handle_throwable(t: Throwable) = {
    if (ASTNode.thrown.get() != t) {
      Debug("Triggered by %s:", getOrigin())
      ASTNode.thrown.set(t)
    }
    throw t
  }
}
