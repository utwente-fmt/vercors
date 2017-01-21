package vct.col.ast

import hre.ast.MessageOrigin;

abstract class ExpressionNode extends ASTNode with BeforeAfterAnnotations {
  /** Block of proof hints to be executed just before
   *  evaluating the expression represented by this AST node.
   *  But after any argument has been evaluated. */
  private[this] var before:BlockStatement = null
  
  /** Block of proof hints to be executed after the
   *  current node has been evaluated. */
  private[this] var after:BlockStatement = null
  
  def set_before(block:BlockStatement) = {
    before = block
    if (block != null) {
      block.setParent(this)
      if (block.getOrigin() == null) {
        block.setOrigin(new MessageOrigin("before block"))
      }
    }
    this
  }
  
  def set_after(block:BlockStatement) = {
    after = block
    if (block != null) {
      block.setParent(this)
      if (block.getOrigin() == null) {
        block.setOrigin(new MessageOrigin("after block"))
      }
    }
    this
  }
  
  def get_before = {
    if (before == null) set_before(new BlockStatement())
    before
  }
  
  def get_after = {
    if (after == null) set_after(new BlockStatement())
    after
  }
}
