package vct.col.ast

import scala.collection.JavaConverters._
import scala.collection.mutable.ArrayBuffer
import vct.col.util.VisitorHelper
import vct.util.ClassName

class AxiomaticDataType(private[this] val _name:String, private[this] val _pars:Array[DeclarationStatement]) extends ASTDeclaration(_name) with VisitorHelper {
  val pars = _pars.clone()
  
  private[this] val _cons = new ArrayBuffer[Method]()
  private[this] val _maps = new ArrayBuffer[Method]()
  private[this] val _axioms = new ArrayBuffer[Axiom]()
  
  def parameters = pars
  def constructors = _cons.toIterable.asJava
  def mappings = _maps.toIterable.asJava
  def axioms = _axioms.toIterable.asJava
  
  @deprecated("this will soon be removed", "next")
  def getParameters = parameters
  
  def add_map(m:Method) : Unit = {
    m.setFlag(ASTFlags.STATIC, true)
    m.setParent(this)
    _maps += m
  }
  
  def add_cons(m:Method) : Unit = {
    m.setFlag(ASTFlags.STATIC, true)
    m.setParent(this)
    _cons += m
  }
  
  def add_axiom(ax:Axiom) : Unit = {
    ax.setParent(this);
    _axioms += ax
  }
  
  override def getDeclName = new ClassName(this.name)
  override def accept_simple[T,A](m:ASTMapping1[T,A], arg:A) = m.map(this, arg)
  override def accept_simple[T](v:ASTVisitor[T]) = handle_standard(() => v.visit(this))
  override def accept_simple[T](m:ASTMapping[T]) = handle_standard(() => m.map(this))
}
