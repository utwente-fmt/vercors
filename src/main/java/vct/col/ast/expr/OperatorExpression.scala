package vct.col.ast.expr

import scala.collection.JavaConverters._
import hre.ast.FileOrigin
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.composite.Hole
import vct.col.ast.util.ASTVisitor
import vct.col.util.{ASTMapping, ASTMapping1, VisitorHelper}

case class OperatorExpression(val operator:StandardOperator, val args:List[ASTNode]) extends ExpressionNode with VisitorHelper {
  require(args != null, "The argument list is null")
  require(operator.arity < 0 || args.length == operator.arity, s"Wrong number of arguments for $operator: got ${args.length}, but expected ${operator.arity}")
  require(args.forall(v => v != null), s"None of the ${args.length} arguments should be null")

  /** Constructs a new operator expression from an array of arguments */
  def this(operator:StandardOperator, args:Array[ASTNode]) = this(operator, args.toList)

  /** Gives a Java wrapper (as a `java.util.List`) for the list of arguments. */
  def argsJava = args.asJava

  /** Yields the first argument, equivalent to `arg(0)`. */
  def first = arg(0)

  /** Yields the first argument, equivalent to `arg(1)`. */
  def second = arg(1)

  /** Yields the third argument, equivalent to `arg(2)`. */
  def third = arg(2)

  /** Yields the number of arguments. Beware, `argslength` takes linear time, not constant time. */
  def argslength = args.length

  /** Either yields the `i`-th argument, or `None` if there is no such argument. */
  def argOption(i:Int) = args.lift(i)

  /** Yields the `i`-th argument, or throws an exception if there is no such argument. */
  def arg(i:Int) = argOption(i) match {
    case None => throw new Error("the operator $operator does not have an argument $i.")
    case Some(argument) => argument
  }

  /** Merges the (file) origins of the leftmost and rightmost argument, provided there
   *  are at least two arguments. */
  def mergeOrigins() = {
    if (args.length < 2) throw new Error("cannot merge on less than 2 arguments.")
    else (args.head.getOrigin, args.last.getOrigin) match {
      case (left:FileOrigin, right:FileOrigin) => setOrigin(left merge right)
      case _ => throw new Error("merging requires both operands to have a FileOrigin.")
    }
  }

  override def `match`(ast:ASTNode) = ast match {
    case OperatorExpression(`operator`, a) => (args corresponds a)(_ `match` _)
    case h:Hole => h `match` this
    case _ => false
  }

  override def accept_simple[T,A](m:ASTMapping1[T,A], arg:A) = m.map(this, arg)
  override def accept_simple[T](v:ASTVisitor[T]) = handle_standard(() => v.visit(this))
  override def accept_simple[T](m:ASTMapping[T]) = handle_standard(() => m.map(this))
  override def isa(op:StandardOperator) = op == operator

  override def debugTreeChildrenFields(): Iterable[String] = Seq("args")
  override def debugTreePropertyFields(): Iterable[String] = Seq("operator")
}
