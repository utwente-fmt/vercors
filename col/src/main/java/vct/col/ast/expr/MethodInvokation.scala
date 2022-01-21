// -*- tab-width:2 ; indent-tabs-mode:nil -*-
// -*- tab-width:2 ; indent-tabs-mode:nil -*-
package vct.col.ast.expr

import vct.col.ast.`type`.ClassType
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.decl.Method
import vct.col.ast.util.{ASTMapping, ASTMapping1, ASTVisitor, VisitorHelper}
import hre.lang.System._

/**
  * This AST node stores method invokations on objects.
  * A function call is seen as invoking a static method on a class.
  *
  * @author sccblom
  *
  */
case class MethodInvokation(`object`: ASTNode, dispatch: ClassType, method: String, var args: Seq[ASTNode]) extends ExpressionNode with VisitorHelper {
  var definition: Method = null

  override def debugTreeChildrenFields = Seq("object", "args")
  override def debugTreePropertyFields = Seq("method", "dispatch")

  def this(`object`: ASTNode, method: String, args: ASTNode*) =
    this(`object`, null, method, args)

  def this(`object`: ASTNode, dispatch: ClassType, method: String, args: Array[ASTNode]) =
    this(`object`, dispatch, method, args.toSeq)

  override def accept_simple[T](visitor: ASTVisitor[T]) = visitor.visit(this)
  override def accept_simple[T](map: ASTMapping[T]) = map.map(this)
  override def accept_simple[R, A](map: ASTMapping1[R, A], arg: A) = map.map(this, arg)

  def getArity: Int = args.length
  def getArg(i: Int): ASTNode = args(i)
  def getArgs: Array[ASTNode] = args.toArray.clone

  def setDefinition(m: Method) = definition = m
  def getDefinition = definition

  /**
    * Check if this invokation is an instantiation.
    */
  def isInstantiation = {
    if (definition == null) Abort("invokation of unknown method")
    definition.kind == Method.Kind.Constructor
  }

  def prependArg(e: ASTNode) = args = Seq(e) ++ args
  def addArg(e: ASTNode) = args = args ++ Seq(e)

  /**
    * @see [[vct.col.ast.stmt.decl.Method.canThrow]]
    */
  def canThrow = getDefinition.canThrow

  /**
    * @see [[vct.col.ast.stmt.decl.Method.canThrowSpec]]
    */
  def canThrowSpec = getDefinition.canThrowSpec
}