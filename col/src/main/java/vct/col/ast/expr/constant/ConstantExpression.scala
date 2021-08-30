package vct.col.ast.expr.constant

import hre.ast.Origin
import vct.col.ast.`type`.{PrimitiveSort, PrimitiveType, Type}
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.composite.Hole
import vct.col.ast.util.{ASTMapping, ASTMapping1, ASTVisitor, VisitorHelper}

/**
 * AST node for wrapping constant values, e.g. integers, booleans, strings, doubles, and longs.
 *
 * @author sccblom, whmoortwijn
 * @param value The constant value that is wrapped by this node.
 */
case class ConstantExpression(val value:Value) extends ASTNode with VisitorHelper {
  def this(v:Value, t:Type) = { this(v); setType(t) }
  def this(v:Value, t:Type, origin:Origin) = { this(v, t); setOrigin(origin) }
  def this(i:Int) = this(new IntegerValue(i), new PrimitiveType(PrimitiveSort.Integer))
  def this(b:Boolean) = this(new BooleanValue(b), new PrimitiveType(PrimitiveSort.Boolean))
  def this(s:String) = this(new StringValue(s), new PrimitiveType(PrimitiveSort.String))
  def this(l:Long) = this(new LongValue(l), new PrimitiveType(PrimitiveSort.Long))
  def this(f:Float) = this(new FloatValue(f), new PrimitiveType(PrimitiveSort.Float))
	def this(d:Double) = this(new DoubleValue(d), new PrimitiveType(PrimitiveSort.Double))

  def this(i:Int, origin:Origin) = { this(i); setOrigin(origin) }
  def this(b:Boolean, origin:Origin) = { this(b); setOrigin(origin) }
  def this(s:String, origin:Origin) = { this(s); setOrigin(origin) }
  def this(l:Long, origin:Origin) = { this(l); setOrigin(origin) }
  def this(f:Float, origin: Origin) = { this(f); setOrigin(origin) }
  def this(d:Double, origin:Origin) = { this(d); setOrigin(origin) }

  override def accept_simple[T,A](m:ASTMapping1[T,A], arg:A) = m.map(this, arg)
  override def accept_simple[T](v:ASTVisitor[T]) = handle_standard(() => v.visit(this))
  override def accept_simple[T](m:ASTMapping[T]) = handle_standard(() => m.map(this))
  override def equals(o:Any) : Boolean = value.equals(o)
  override def isConstant(o:Any) : Boolean = equals(o)
  override def toString() : String = value.toString()

  override def `match`(ast:ASTNode) = ast match {
    case h:Hole => h.`match`(this)
    case _ => ast.equals(value)
  }

  override def debugTreeChildrenFields: Iterable[String] = Seq()
  override def debugTreePropertyFields: Iterable[String] = Seq("value")
}
