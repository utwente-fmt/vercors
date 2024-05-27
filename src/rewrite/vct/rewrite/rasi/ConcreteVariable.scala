package vct.rewrite.rasi

import vct.col.ast._

/** A variable whose value can possibly be resolved in an abstract state.
  */
sealed trait ResolvableVariable[G] {

  /** Determines whether this variable corresponds to the given expression in
    * the given state.
    *
    * @param expr
    *   COL expression
    * @param state
    *   Abstract state to evaluate the expression in
    * @return
    *   <code>true</code> if the expression represents this variable,
    *   <code>false</code> otherwise
    */
  def is(expr: Expr[G], state: AbstractState[G]): Boolean

  /** Determines whether this variable is contained in the given expression in
    * the given state, e.g. if this variable represents an index in a collection
    * and the expression represents the collection.
    *
    * @param expr
    *   COL expression
    * @param state
    *   Abstract state to evaluate the expression in
    * @return
    *   <code>true</code> if the object represented by the expression contains
    *   this variable, <code>false</code> otherwise
    */
  def is_contained_by(expr: Expr[G], state: AbstractState[G]): Boolean

  /** Returns the type of this variable.
    *
    * @return
    *   The COL type of this variable
    */
  def t: Type[G]
}

/** A virtual variable representing a subroutine return.
  */
case class ResultVariable[G](return_type: Type[G])
    extends ResolvableVariable[G] {
  override def is(expr: Expr[G], state: AbstractState[G]): Boolean =
    expr match {
      case AmbiguousResult() | Result(_) => true
      case _ => false
    }
  override def is_contained_by(
      expr: Expr[G],
      state: AbstractState[G],
  ): Boolean = is(expr, state)
  override def t: Type[G] = return_type
}

/** A variable that can be tracked in the abstract state.
  */
sealed trait ConcreteVariable[G] extends ResolvableVariable[G] {

  /** Creates an expression that represents this variable in COL.
    *
    * @return
    *   A COL expression representing this variable
    */
  def to_expression: Expr[G]

  protected def field_equals(expr: Expr[G], field: InstanceField[G]): Boolean =
    expr match {
      // TODO: Support other types of expressions? Take object into account?
      case Deref(_, f) => f.decl.equals(field)
      case _ => false
    }

  protected def variable_equals(expr: Expr[G], variable: Variable[G]): Boolean =
    expr match {
      // TODO: Are there other ways to refer to variables?
      case Local(ref) => ref.decl.equals(variable)
      case _ => false
    }

  /** Defines an ordering among concrete variables.
    *
    * @param other
    *   Variable to be compared to
    * @return
    *   <code>true</code> if <code>this > other</code>, <code>false</code>
    *   otherwise
    */
  def compare(other: ConcreteVariable[G]): Boolean
}

/** A variable that represents a local variable in the COL system.
  */
case class LocalVariable[G](variable: Variable[G]) extends ConcreteVariable[G] {
  override def is(expr: Expr[G], state: AbstractState[G]): Boolean =
    variable_equals(expr, variable)
  override def is_contained_by(
      expr: Expr[G],
      state: AbstractState[G],
  ): Boolean = is(expr, state)
  override def to_expression: Expr[G] = Local[G](variable.ref)(variable.o)
  override def t: Type[G] = variable.t
  override def compare(other: ConcreteVariable[G]): Boolean =
    other match {
      case LocalVariable(v) => v.toInlineString > variable.toInlineString
      case _ => false
    }
}

/** A variable representing a field (attribute) of a COL class.
  */
case class FieldVariable[G](field: InstanceField[G])
    extends ConcreteVariable[G] {
  override def is(expr: Expr[G], state: AbstractState[G]): Boolean =
    field_equals(expr, field)
  override def is_contained_by(
      expr: Expr[G],
      state: AbstractState[G],
  ): Boolean = is(expr, state)
  override def to_expression: Expr[G] =
    Deref[G](AmbiguousThis()(field.o), field.ref)(field.o)(field.o)
  override def t: Type[G] = field.t
  override def compare(other: ConcreteVariable[G]): Boolean =
    other match {
      case LocalVariable(_) => true
      case FieldVariable(f) => f.toInlineString > field.toInlineString
      case SizeVariable(_) => false
      case IndexedVariable(_, _) => false
    }
}

// TODO: Generalize size and indexed variables for local variables

/** A variable representing the size of a collection.
  */
case class SizeVariable[G](field: InstanceField[G])
    extends ConcreteVariable[G] {
  override def is(expr: Expr[G], state: AbstractState[G]): Boolean =
    expr match {
      case Size(obj) => field_equals(obj, field)
      case _ => false
    }
  override def is_contained_by(
      expr: Expr[G],
      state: AbstractState[G],
  ): Boolean = is(expr, state) || field_equals(expr, field)
  override def to_expression: Expr[G] =
    Size(Deref[G](AmbiguousThis()(field.o), field.ref)(field.o)(field.o))(
      field.o
    )
  override def t: Type[G] = TInt()(field.o)
  override def compare(other: ConcreteVariable[G]): Boolean =
    other match {
      case LocalVariable(_) => true
      case FieldVariable(_) => true
      case SizeVariable(f) => f.toInlineString > field.toInlineString
      case IndexedVariable(_, _) => false
    }
}

/** A variable representing an index of a collection.
  */
case class IndexedVariable[G](field: InstanceField[G], i: Int)
    extends ConcreteVariable[G] {
  override def is(expr: Expr[G], state: AbstractState[G]): Boolean =
    expr match {
      case AmbiguousSubscript(collection, index) =>
        field_equals(collection, field) &&
        i == state.resolve_integer_expression(index).try_to_resolve()
          .getOrElse(-1)
      case SeqSubscript(seq, index) =>
        field_equals(seq, field) && i == state.resolve_integer_expression(index)
          .try_to_resolve().getOrElse(-1)
      case ArraySubscript(arr, index) =>
        field_equals(arr, field) && i == state.resolve_integer_expression(index)
          .try_to_resolve().getOrElse(-1)
      case PointerSubscript(pointer, index) =>
        field_equals(pointer, field) &&
        i == state.resolve_integer_expression(index).try_to_resolve()
          .getOrElse(-1)
      case _ => false
    }
  override def is_contained_by(
      expr: Expr[G],
      state: AbstractState[G],
  ): Boolean =
    expr match {
      // TODO: What about nested drops/takes?
      case Drop(xs, count) =>
        field_equals(xs, field) && state.resolve_integer_expression(count)
          .try_to_resolve().getOrElse(i + 1) < i
      case Take(xs, count) =>
        field_equals(xs, field) && state.resolve_integer_expression(count)
          .try_to_resolve().getOrElse(i - 1) >= i
      case Slice(xs, from, to) =>
        field_equals(xs, field) && state.resolve_integer_expression(from)
          .try_to_resolve().getOrElse(i + 1) < i &&
        state.resolve_integer_expression(to).try_to_resolve()
          .getOrElse(i - 1) >= i
      case _ => field_equals(expr, field) || is(expr, state)
    }
  override def to_expression: Expr[G] =
    field.t match {
      case TSeq(_) =>
        SeqSubscript(
          Deref[G](AmbiguousThis()(field.o), field.ref)(field.o)(field.o),
          IntegerValue(i)(field.o),
        )(field.o)(field.o)
      case TArray(_) =>
        ArraySubscript(
          Deref[G](AmbiguousThis()(field.o), field.ref)(field.o)(field.o),
          IntegerValue(i)(field.o),
        )(field.o)(field.o)
      case TPointer(_) =>
        PointerSubscript(
          Deref[G](AmbiguousThis()(field.o), field.ref)(field.o)(field.o),
          IntegerValue(i)(field.o),
        )(field.o)(field.o)
    }
  override def t: Type[G] =
    field.t match {
      case TSeq(element) => element
      case TArray(element) => element
      case TPointer(element) => element
    }
  override def compare(other: ConcreteVariable[G]): Boolean =
    other match {
      case LocalVariable(_) => true
      case FieldVariable(_) => true
      case SizeVariable(_) => true
      case IndexedVariable(f, ind) =>
        if (f != field)
          f.toInlineString > field.toInlineString
        else
          ind > i
    }
}
