package vct.col.ast.stmt.decl

import vct.col.ast.generic.ASTNode
import vct.col.ast.util.ClassName


/**
 * Abstract AST node that represents a declaration.
 * 
 * @param name Contains the name of the class/module/package.
 */
abstract class ASTDeclaration(val name:String) extends ASTNode {
  /** Contains the root of the source forest. */
  protected var root:ProgramUnit = null
  
  /** Contains the package name. */
  protected var packageName:ClassName = null
  
  def getDeclName : ClassName
  def packageDefined = packageName != null
  
  def attach(root:ProgramUnit, packageName:ClassName) = {
    this.packageName = packageName
    this.root = root
  }
}
