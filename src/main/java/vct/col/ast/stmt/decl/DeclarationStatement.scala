package vct.col.ast.stmt.decl

import hre.lang.System.Debug
import vct.col.ast.`type`.Type
import vct.col.ast.generic.ASTNode
import vct.col.ast.util.ASTVisitor
import vct.col.util.{ASTMapping, ASTMapping1, VisitorHelper}
import vct.util.ClassName

/**
 * AST node that represents a declaration statement, for example "`int test := 2+4;`".
 *
 * @author sccblom, whmoortwijn
 * @param name The name of the declared variable, e.g. "`test`".
 * @param type The type of the declared variable, e.g. "`int`".
 * @param init Optionally, an expression that determines the initial value of the declared variable, e.g. "`2+4`".
 */
case class DeclarationStatement(override val name:String, val `type`:Type, val init:Option[ASTNode]) extends ASTDeclaration(name) with VisitorHelper {
  /**
   * Initialises a new AST node that represents a declaration statement without
   * initial value, for example "`int test;`".
   */
  def this(name:String, t:Type) = this(name, t, None)

  /** Builds a new declaration from an initial expression (`init`) that is possibly `null`. */
  def this(name:String, t:Type, init:ASTNode) = this(name, t, Option(init))

  /** Yields the declaration type */
  override def getType = `type`

  /** Java wrapper over the initial expression, which may return `null` (in case `init` equals `None`).  */
  def initJava = init match {
    case Some(node) => node
    case None => null
  }

  /** Yields the full name of the declared variable, including package name. */
  override def getDeclName = {
    Debug(s"$packageName.$name")
    new ClassName(packageName, name)
  }

  override def accept_simple[T,A](m:ASTMapping1[T,A], arg:A) = m.map(this, arg)
  override def accept_simple[T](v:ASTVisitor[T]) = handle_standard(() => v.visit(this))
  override def accept_simple[T](m:ASTMapping[T]) = handle_standard(() => m.map(this))

  override def debugTreeChildrenFields(): Iterable[String] = Seq("type", "init")
  override def debugTreePropertyFields(): Iterable[String] = Seq("name")
}
