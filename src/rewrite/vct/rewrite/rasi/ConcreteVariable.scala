package vct.rewrite.rasi

import vct.col.ast._

trait ConcreteVariable[G] {
  def is(expr: Expr[G]): Boolean
  def extract_from_expression(expr: Expr[G]): Field[G] = expr match {
    // TODO: Need to also consider the object!
    case Deref(obj, ref) => ref.decl
  }
}

case class FieldVariable[G](field: Field[G]) extends ConcreteVariable[G] {
  override def is(expr: Expr[G]): Boolean = field.equals(extract_from_expression(expr))
}

case class IndexedVariable[G](field: Field[G], i: Int) extends ConcreteVariable[G] {
  override def is(expr: Expr[G]): Boolean = expr match {
    case AmbiguousSubscript(collection, index) => field.equals(extract_from_expression(collection)) && i == Utils.resolve_integer_expression(index)
    case SeqSubscript(seq, index) => field.equals(extract_from_expression(seq)) && i == Utils.resolve_integer_expression(index)
    case ArraySubscript(arr, index) => field.equals(extract_from_expression(arr)) && i == Utils.resolve_integer_expression(index)
    case PointerSubscript(pointer, index) => field.equals(extract_from_expression(pointer)) && i == Utils.resolve_integer_expression(index)
  }
}
