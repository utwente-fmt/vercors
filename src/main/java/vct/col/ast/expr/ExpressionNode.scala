package vct.col.ast.expr

import hre.ast.MessageOrigin
import vct.col.ast.generic.{ASTNode, BeforeAfterAnnotations}
import vct.col.ast.stmt.composite.BlockStatement;

abstract class ExpressionNode extends ASTNode with BeforeAfterAnnotations {
  /** 
   *  Block of proof hints to be executed just before
   *  evaluating the expression represented by this AST node.
   *  But after any argument has been evaluated. 
   */
  private var before : Option[BlockStatement] = None
  
  /** 
   *  Block of proof hints to be executed after the
   *  current node has been evaluated.
   */
  private var after : Option[BlockStatement] = None
  
  /** Configures the given block by assigning a parent and origin */
  private def configureBlock(block:BlockStatement, origin:String) : BeforeAfterAnnotations = {
    block.setParent(this)
    Option(block.getOrigin()) match {
      case None => block.setOrigin(new MessageOrigin(origin)); this
      case Some(_) => this
    }
  }
  
  /** Configures `block` if it contains a block statement */
  private def configureBlock(block:Option[BlockStatement], origin:String) : BeforeAfterAnnotations = block match {
    case Some(b) => configureBlock(b, origin)
    case None => this
  }
  
  /** Assigns the given `block` to `before` */
  def set_before(block:Option[BlockStatement]) = {
    before = block
    configureBlock(before, "before block")
  }
  
  /** Assigns the given `block` to `after` */
  def set_after(block:Option[BlockStatement]) = {
    after = block
    configureBlock(after, "after block")
  }
  
  /** Assigns the given `block` to `before` */
  def set_before(block:BlockStatement) = set_before(Option(block))
  
  /** Assigns the given `block` to `after` */
  def set_after(block:BlockStatement) = set_after(Option(block))
  
  /** Clears the value of `before` */
  def clearBefore = set_before(None)
  
  /** Clears the value of `after` */
  def clearAfter = set_after(None)
  
  /** Assigns a default value to `before` and thereby resetting it
   *  @return the default block statement assigned to `before` */
  def resetBefore = {
    var block = new BlockStatement()
    set_before(block)
    block
  }
  
  /** Assigns a default value to `after` and thereby resetting it
   *  @return the default block statement assigned to `after` */
  def resetAfter = {
    var block = new BlockStatement()
    set_after(block)
    block
  }
  
  /** Gives the value of `before` if it holds a statement, otherwise a default statement is returned */
  def get_before = before match {
    case None => resetBefore
    case Some(block) => block
  }

  /** Gives the value of `after` if it holds a statement, otherwise a default statement is returned */
  def get_after = after match {
    case None => resetAfter
    case Some(block) => block
  }
}
