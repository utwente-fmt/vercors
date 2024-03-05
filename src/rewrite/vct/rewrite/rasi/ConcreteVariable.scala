package vct.rewrite.rasi

import vct.col.ast._

trait ConcreteVariable[G] {
  def is(expr: Expr[G], state: AbstractState[G]): Boolean
  def is_contained_by(expr: Expr[G], state: AbstractState[G]): Boolean
  def to_expression: Expr[G]
  def t: Type[G]
  def field_equals(expr: Expr[G], field: InstanceField[G]): Boolean = expr match {
    // TODO: Support other types of expressions? Take object into account?
    case Deref(_, f) => f.decl.equals(field)
    case _ => false
  }
}

case class FieldVariable[G](field: InstanceField[G]) extends ConcreteVariable[G] {
  override def is(expr: Expr[G], state: AbstractState[G]): Boolean = field_equals(expr, field)
  override def is_contained_by(expr: Expr[G], state: AbstractState[G]): Boolean = field_equals(expr, field)
  override def to_expression: Expr[G] = Deref[G](AmbiguousThis()(field.o), field.ref)(field.o)(field.o)
  override def t: Type[G] = field.t
}

case class IndexedVariable[G](field: InstanceField[G], i: Int) extends ConcreteVariable[G] {
  override def is(expr: Expr[G], state: AbstractState[G]): Boolean = expr match {
    case AmbiguousSubscript(collection, index) => field_equals(collection, field) && i == state.resolve_integer_expression(index).try_to_resolve().getOrElse(-1)
    case SeqSubscript(seq, index) => field_equals(seq, field) && i == state.resolve_integer_expression(index).try_to_resolve().getOrElse(-1)
    case ArraySubscript(arr, index) => field_equals(arr, field) && i == state.resolve_integer_expression(index).try_to_resolve().getOrElse(-1)
    case PointerSubscript(pointer, index) => field_equals(pointer, field) && i == state.resolve_integer_expression(index).try_to_resolve().getOrElse(-1)
    case _ => false
  }
  override def is_contained_by(expr: Expr[G], state: AbstractState[G]): Boolean = expr match {
    // TODO: What about slices?
    case _ => field_equals(expr, field)
  }
  override def to_expression: Expr[G] = field.t match {
    case TSeq(_) => SeqSubscript(Deref[G](AmbiguousThis()(field.o), field.ref)(field.o)(field.o), IntegerValue(i)(field.o))(field.o)(field.o)
    case TArray(_) => ArraySubscript(Deref[G](AmbiguousThis()(field.o), field.ref)(field.o)(field.o), IntegerValue(i)(field.o))(field.o)(field.o)
    case TPointer(_) => PointerSubscript(Deref[G](AmbiguousThis()(field.o), field.ref)(field.o)(field.o), IntegerValue(i)(field.o))(field.o)(field.o)
  }
  override def t: Type[G] = field.t match {
    case TSeq(element) => element
    case TArray(element) => element
    case TPointer(element) => element
  }
}
