package vct.rewrite.rasi

import vct.col.ast._
import vct.col.origin.Origin

sealed trait ResolvableVariable[G] {
  def is(expr: Expr[G], state: AbstractState[G]): Boolean
  def is_contained_by(expr: Expr[G], state: AbstractState[G]): Boolean
  def t: Type[G]
}

case class ResultVariable[G](return_type: Type[G]) extends ResolvableVariable[G] {
  override def is(expr: Expr[G], state: AbstractState[G]): Boolean = expr match {
    case AmbiguousResult() | Result(_) => true
    case _ => false
  }
  override def is_contained_by(expr: Expr[G], state: AbstractState[G]): Boolean = is(expr, state)
  override def t: Type[G] = return_type
}

sealed trait ConcreteVariable[G] extends ResolvableVariable[G] {
  def to_expression: Expr[G]
  def field_equals(expr: Expr[G], field: InstanceField[G]): Boolean = expr match {
    // TODO: Support other types of expressions? Take object into account?
    case Deref(_, f) => f.decl.equals(field)
    case _ => false
  }
  def compare(other: ConcreteVariable[G]): Boolean
}

case class FieldVariable[G](field: InstanceField[G]) extends ConcreteVariable[G] {
  override def is(expr: Expr[G], state: AbstractState[G]): Boolean = field_equals(expr, field)
  override def is_contained_by(expr: Expr[G], state: AbstractState[G]): Boolean = is(expr, state)
  override def to_expression: Expr[G] = Deref[G](AmbiguousThis()(field.o), field.ref)(field.o)(field.o)
  override def t: Type[G] = field.t
  override def compare(other: ConcreteVariable[G]): Boolean = other match {
    case FieldVariable(f) => f.toInlineString > field.toInlineString
    case SizeVariable(_) => false
    case IndexedVariable(_, _) => false
  }
}

case class SizeVariable[G](field: InstanceField[G]) extends ConcreteVariable[G] {
  override def is(expr: Expr[G], state: AbstractState[G]): Boolean = expr match {
    case Size(obj) => field_equals(obj, field)
    case _ => false
  }
  override def is_contained_by(expr: Expr[G], state: AbstractState[G]): Boolean = field_equals(expr, field)
  override def to_expression: Expr[G] = Size(Deref[G](AmbiguousThis()(field.o), field.ref)(field.o)(field.o))(field.o)
  override def t: Type[G] = TInt()(field.o)
  override def compare(other: ConcreteVariable[G]): Boolean = other match {
    case FieldVariable(_) => true
    case SizeVariable(f) => f.toInlineString > field.toInlineString
    case IndexedVariable(_, _) => false
  }
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
  override def compare(other: ConcreteVariable[G]): Boolean = other match {
    case FieldVariable(_) => true
    case SizeVariable(_) => true
    case IndexedVariable(f, ind) =>
      if (f != field) f.toInlineString > field.toInlineString
      else ind > i
  }
}
