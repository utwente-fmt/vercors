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
object ResolvableVariable {
  def single_from[G](expr: Expr[G]): ResolvableVariable[G] =
    expr match {
      case _: AmbiguousResult[_] | _: Result[_] => ResultSimpleVariable(expr.t)
      case Deref(_, ref) => FieldSimpleVariable(ref.decl)
      case Local(ref) => LocalSimpleVariable(ref.decl)
      case _ =>
        throw new IllegalArgumentException(
          "Cannot synthesize variable from expression" + expr.toInlineString
        )
    }
  def indexed_from[G](expr: Expr[G], index: Int): ResolvableVariable[G] =
    expr match {
      case _: AmbiguousResult[G] | _: Result[G] =>
        ResultIndexedVariable(expr.t, index)
      case Deref(_, ref) => FieldIndexedVariable(ref.decl, index)
      case Local(ref) => LocalIndexedVariable(ref.decl, index)
      case _ =>
        throw new IllegalArgumentException(
          "Cannot synthesize indexed variable from expression" +
            expr.toInlineString
        )
    }
  def size_from[G](expr: Expr[G]): ResolvableVariable[G] =
    expr match {
      case _: AmbiguousResult[_] | _: Result[_] => ResultSizeVariable(expr.t)
      case Deref(_, ref) => FieldSizeVariable(ref.decl)
      case Local(ref) => LocalSizeVariable(ref.decl)
      case _ =>
        throw new IllegalArgumentException(
          "Cannot synthesize size variable from expression" +
            expr.toInlineString
        )
    }
  def from[G](expr: Expr[G], resolve: Expr[G] => Int): ResolvableVariable[G] =
    expr match {
      case Old(e, _) => from(e, resolve)
      case AmbiguousSubscript(collection, index) =>
        indexed_from(collection, resolve(index))
      case SeqSubscript(seq, index) => indexed_from(seq, resolve(index))
      case ArraySubscript(arr, index) => indexed_from(arr, resolve(index))
      case Size(obj) => size_from(obj)
      case Length(arr) => size_from(arr)
      case _ => single_from(expr)
    }
}

sealed trait SimpleVariable[G] extends ResolvableVariable[G]
sealed trait SizeVariable[G] extends ResolvableVariable[G]
sealed trait IndexedVariable[G] extends ResolvableVariable[G] {
  def index: Int

  protected def indexed_equals(
      expr: Expr[G],
      decl_matches: Expr[G] => Boolean,
      state: AbstractState[G],
  ): Boolean =
    expr match {
      case AmbiguousSubscript(collection, idx) =>
        decl_matches(collection) &&
        index == state.resolve_integer_expression(idx).try_to_resolve()
          .getOrElse(-1)
      case SeqSubscript(seq, idx) =>
        decl_matches(seq) && index == state.resolve_integer_expression(idx)
          .try_to_resolve().getOrElse(-1)
      case ArraySubscript(arr, idx) =>
        decl_matches(arr) && index == state.resolve_integer_expression(idx)
          .try_to_resolve().getOrElse(-1)
      case PointerSubscript(pointer, idx) =>
        decl_matches(pointer) && index == state.resolve_integer_expression(idx)
          .try_to_resolve().getOrElse(-1)
      case _ => false
    }

  protected def index_contained_in(
      expr: Expr[G],
      decl_matches: Expr[G] => Boolean,
      state: AbstractState[G],
  ): Boolean =
    expr match {
      // TODO: What about nested drops/takes?
      case Drop(xs, count) =>
        decl_matches(xs) && state.resolve_integer_expression(count)
          .try_to_resolve().getOrElse(index + 1) < index
      case Take(xs, count) =>
        decl_matches(xs) && state.resolve_integer_expression(count)
          .try_to_resolve().getOrElse(index - 1) >= index
      case Slice(xs, from, to) =>
        decl_matches(xs) && state.resolve_integer_expression(from)
          .try_to_resolve().getOrElse(index + 1) < index &&
        state.resolve_integer_expression(to).try_to_resolve()
          .getOrElse(index - 1) >= index
      case _ => decl_matches(expr) || indexed_equals(expr, decl_matches, state)
    }

  protected def generate_expression(
      t: Type[G],
      coll: Expr[G],
      i: Int,
  ): Expr[G] =
    t match {
      case TSeq(_) =>
        SeqSubscript(coll, IntegerValue(i)(coll.o))(coll.o)(coll.o)
      case TArray(_) =>
        ArraySubscript(coll, IntegerValue(i)(coll.o))(coll.o)(coll.o)
      case TPointer(_) =>
        PointerSubscript(coll, IntegerValue(i)(coll.o))(coll.o)(coll.o)
    }
}

/** A virtual variable representing a subroutine return.
  */
sealed trait ResultVariable[G] extends ResolvableVariable[G] {
  protected def is_result(expr: Expr[G]): Boolean =
    expr match {
      case AmbiguousResult() | Result(_) => true
      case _ => false
    }
}

case class ResultSimpleVariable[G](return_type: Type[G])
    extends ResultVariable[G] with SimpleVariable[G] {
  override def is(expr: Expr[G], state: AbstractState[G]): Boolean =
    is_result(expr)

  override def is_contained_by(
      expr: Expr[G],
      state: AbstractState[G],
  ): Boolean = is_result(expr)

  override def t: Type[G] = return_type
}

case class ResultSizeVariable[G](return_type: Type[G])
    extends ResultVariable[G] with SizeVariable[G] {
  override def is(expr: Expr[G], state: AbstractState[G]): Boolean =
    expr match {
      case Size(obj) if is_result(obj) => true
      case _ => false
    }

  override def is_contained_by(
      expr: Expr[G],
      state: AbstractState[G],
  ): Boolean = is(expr, state) || is_result(expr)

  override def t: Type[G] = return_type
}

case class ResultIndexedVariable[G](return_type: Type[G], i: Int)
    extends ResultVariable[G] with IndexedVariable[G] {
  override def index: Int = i

  override def is(expr: Expr[G], state: AbstractState[G]): Boolean =
    indexed_equals(expr, e => is_result(e), state)

  override def is_contained_by(
      expr: Expr[G],
      state: AbstractState[G],
  ): Boolean = index_contained_in(expr, e => is_result(e), state)

  override def t: Type[G] = return_type
}

/** A variable that tracks a concrete variable, either a local variable or a
  * class attribute, in the source code.
  */
sealed trait ConcreteVariable[G] extends ResolvableVariable[G] {

  /** Creates an expression that represents this variable in COL.
    *
    * @return
    *   A COL expression representing this variable
    */
  def to_expression(obj: Option[Expr[G]]): Expr[G]

  def get_declaration: Declaration[G]

  /** Defines an ordering among concrete variables, to create a predictable and
    * repeatable order for output.
    *
    * @param other
    *   Variable to be compared to
    * @return
    *   <code>true</code> if <code>this > other</code>, <code>false</code>
    *   otherwise
    */
  def compare(other: ConcreteVariable[G]): Boolean
}

sealed trait LocalVariable[G] extends ConcreteVariable[G] {
  def v: Variable[G]

  override def get_declaration: Declaration[G] = v

  protected def variable_equals(expr: Expr[G]): Boolean =
    expr match {
      case Local(ref) => ref.decl.equals(v)
      case _ => false
    }
}

/** A variable that represents a local variable in the COL system.
  */
case class LocalSimpleVariable[G](variable: Variable[G])
    extends LocalVariable[G] with SimpleVariable[G] {
  override def v: Variable[G] = variable

  override def is(expr: Expr[G], state: AbstractState[G]): Boolean =
    variable_equals(expr)

  override def is_contained_by(
      expr: Expr[G],
      state: AbstractState[G],
  ): Boolean = is(expr, state)

  override def to_expression(obj: Option[Expr[G]]): Expr[G] =
    Local[G](variable.ref)(variable.o)

  override def t: Type[G] = variable.t

  override def compare(other: ConcreteVariable[G]): Boolean =
    other match {
      case LocalSimpleVariable(v) => v.toInlineString > variable.toInlineString
      case _ => false
    }
}

case class LocalSizeVariable[G](seq: Variable[G])
    extends LocalVariable[G] with SizeVariable[G] {
  override def v: Variable[G] = seq

  override def is(expr: Expr[G], state: AbstractState[G]): Boolean =
    expr match {
      case Size(obj) => variable_equals(obj)
      case _ => false
    }

  override def is_contained_by(
      expr: Expr[G],
      state: AbstractState[G],
  ): Boolean = is(expr, state) || variable_equals(expr)

  override def to_expression(obj: Option[Expr[G]]): Expr[G] =
    Size(Local[G](seq.ref)(seq.o))(seq.o)

  override def t: Type[G] = TInt()(seq.o)

  override def compare(other: ConcreteVariable[G]): Boolean =
    other match {
      case LocalSimpleVariable(_) => true
      case LocalIndexedVariable(_, _) => true
      case LocalSizeVariable(s) => s.toInlineString > seq.toInlineString
      case _ => false
    }
}

case class LocalIndexedVariable[G](seq: Variable[G], i: Int)
    extends LocalVariable[G] with IndexedVariable[G] {
  override def v: Variable[G] = seq

  override def index: Int = i

  override def is(expr: Expr[G], state: AbstractState[G]): Boolean =
    indexed_equals(expr, e => variable_equals(e), state)

  override def is_contained_by(
      expr: Expr[G],
      state: AbstractState[G],
  ): Boolean = index_contained_in(expr, e => variable_equals(e), state)

  override def to_expression(obj: Option[Expr[G]]): Expr[G] =
    generate_expression(seq.t, Local[G](seq.ref)(seq.o), i)

  override def t: Type[G] =
    seq.t match {
      case TSeq(element) => element
      case TArray(element) => element
      case TPointer(element) => element
    }

  override def compare(other: ConcreteVariable[G]): Boolean =
    other match {
      case _: LocalSimpleVariable[G] => true
      case LocalIndexedVariable(s, index) =>
        if (s != seq)
          s.toInlineString > seq.toInlineString
        else
          index > i
      case _ => false
    }
}

sealed trait FieldVariable[G] extends ConcreteVariable[G] {
  def f: InstanceField[G]

  override def get_declaration: Declaration[G] = f

  protected def field_equals(expr: Expr[G]): Boolean =
    expr match {
      // TODO: Support other types of expressions? Take object into account?
      case Deref(_, ref) => ref.decl.equals(f)
      case _ => false
    }
}

/** A variable representing a field (attribute) of a COL class.
  */
case class FieldSimpleVariable[G](field: InstanceField[G])
    extends FieldVariable[G] with SimpleVariable[G] {
  override def f: InstanceField[G] = field

  override def is(expr: Expr[G], state: AbstractState[G]): Boolean =
    field_equals(expr)

  override def is_contained_by(
      expr: Expr[G],
      state: AbstractState[G],
  ): Boolean = is(expr, state)

  override def to_expression(obj: Option[Expr[G]]): Expr[G] =
    Deref[G](obj.getOrElse(AmbiguousThis()(field.o)), field.ref)(field.o)(
      field.o
    )

  override def t: Type[G] = field.t

  override def compare(other: ConcreteVariable[G]): Boolean =
    other match {
      case _: LocalVariable[G] => true
      case FieldSimpleVariable(f) => f.toInlineString > field.toInlineString
      case FieldSizeVariable(_) => false
      case FieldIndexedVariable(_, _) => false
    }
}

/** A variable representing the size of a collection.
  */
case class FieldSizeVariable[G](field: InstanceField[G])
    extends FieldVariable[G] with SizeVariable[G] {
  override def f: InstanceField[G] = field

  override def is(expr: Expr[G], state: AbstractState[G]): Boolean =
    expr match {
      case Size(obj) => field_equals(obj)
      case _ => false
    }

  override def is_contained_by(
      expr: Expr[G],
      state: AbstractState[G],
  ): Boolean = is(expr, state) || field_equals(expr)

  override def to_expression(obj: Option[Expr[G]]): Expr[G] =
    Size(Deref[G](obj.getOrElse(AmbiguousThis()(field.o)), field.ref)(field.o)(
      field.o
    ))(field.o)

  override def t: Type[G] = TInt()(field.o)

  override def compare(other: ConcreteVariable[G]): Boolean =
    other match {
      case _: LocalVariable[G] => true
      case FieldSimpleVariable(_) => true
      case FieldIndexedVariable(_, _) => true
      case FieldSizeVariable(f) => f.toInlineString > field.toInlineString
    }
}

/** A variable representing an index of a collection.
  */
case class FieldIndexedVariable[G](field: InstanceField[G], i: Int)
    extends FieldVariable[G] with IndexedVariable[G] {
  override def f: InstanceField[G] = field

  override def index: Int = i

  override def is(expr: Expr[G], state: AbstractState[G]): Boolean =
    indexed_equals(expr, e => field_equals(e), state)

  override def is_contained_by(
      expr: Expr[G],
      state: AbstractState[G],
  ): Boolean = index_contained_in(expr, e => field_equals(e), state)

  override def to_expression(obj: Option[Expr[G]]): Expr[G] = {
    generate_expression(
      field.t,
      Deref[G](obj.getOrElse(AmbiguousThis()(field.o)), field.ref)(field.o)(
        field.o
      ),
      i,
    )
  }

  override def t: Type[G] =
    field.t match {
      case TSeq(element) => element
      case TArray(element) => element
      case TPointer(element) => element
    }

  override def compare(other: ConcreteVariable[G]): Boolean =
    other match {
      case _: LocalVariable[G] => true
      case FieldSimpleVariable(_) => true
      case FieldIndexedVariable(f, ind) =>
        if (f != field)
          f.toInlineString > field.toInlineString
        else
          ind > i
      case FieldSizeVariable(_) => false
    }
}
