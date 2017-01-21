package vct.col.util

import hre.ast.Origin
import vct.col.ast.ASTNode

trait VisitorHelper {
  def getOrigin() : Origin
  
  /**
   * This function is used in many AST classes to handle/print exceptions when 
   * executing a visitor pattern over the AST.
   */
  def handle_throwable(t:Throwable) = {
    if (ASTNode.thrown.get() != t) {
      System.err.printf("Triggered by %s:%n", getOrigin())
      ASTNode.thrown.set(t)
    }
		throw t
  }
  
  def handle_standard[T](fun:()=>T) = try fun() catch { 
    case t:Throwable => handle_throwable(t) 
  }
}