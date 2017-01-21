package vct.col.ast

import hre.ast.FileOrigin
import scala.annotation.varargs
import vct.col.util.VisitorHelper

/**
 * Wytse: I used an extra static factory here to retain the varargs functionality. Without the
 * "varargs" annotation the Java code breaks, and apparently constructors cannot have "varargs" annotations..
 */
object OperatorExpression {
  @varargs def construct(op:StandardOperator, args:ASTNode*) = new OperatorExpression(op, args:_*)
}

class OperatorExpression private[this] (val operator:StandardOperator, private[this] val _args:Array[ASTNode]) extends ExpressionNode with VisitorHelper {
  val args : Array[ASTNode] = _args.clone()
  
  def arg(i:Int) : ASTNode =
    if (i >= args.length) 
      throw new Error(String.format("The operator %s does not have an argument %i.", List(operator, i)))
    else args.apply(i)
  
  /** Use {@code OperatorExpression.construct(...)} to construct a new operator expression. */
  private def this(op:StandardOperator, args:ASTNode*) = {
    this(op, args.toArray)
    if (op.arity() >= 0 && args.length != op.arity())
      hre.lang.System.Abort("Wrong number of arguments for %s: got %d, but expected %d.", 
          List(operator, _args.length, operator.arity()))
  }
  
  def mergeOrigins() = {
    if (args.length < 2) 
      throw new Error("Cannot merge on less than 2 arguments.")
    if (!args.head.getOrigin.isInstanceOf[FileOrigin]) 
      throw new Error("The merge requires the left operator to have a FileOrigin.")
    if (!args.last.getOrigin.isInstanceOf[FileOrigin]) 
      throw new Error("The merge requires the right operator to have a FileOrigin.")
    
    var start = args.head.getOrigin.asInstanceOf[FileOrigin]
    var end = args.last.getOrigin.asInstanceOf[FileOrigin]
    setOrigin(start.merge(end))
  }
  
  override def isa(op:StandardOperator) = op == this.operator
  override def accept_simple[T,A](m:ASTMapping1[T,A], arg:A) = m.map(this, arg)
  override def accept_simple[T](v:ASTVisitor[T]) = handle_standard(() => v.visit(this))
  override def accept_simple[T](m:ASTMapping[T]) = handle_standard(() => m.map(this))
  
  private[this] def match_compare(a:Array[ASTNode], b:Array[ASTNode]) : Boolean = {
    if (a.isEmpty || b.isEmpty) true
    else if (a.head.`match`(b.head)) match_compare(a.tail, b.tail)
    else false
  }
  
  override def `match`(ast:ASTNode) = ast match {
    case h:Hole => h.`match`(this)
    case oe:OperatorExpression => if (oe.isa(operator)) match_compare(_args, oe.args) else false
    case _ => false
  }
}
