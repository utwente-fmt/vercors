package vct.col.ast.`type`

import hre.lang.System.Abort
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.decl.{NameSpace, ProgramUnit}
import vct.col.ast.util.{ExternalClassLoader, TypeMapping}

import scala.jdk.CollectionConverters._

/**
 * Subclass of ASTNode meant for holding all type expressions.
 *
 * Types need to be both manipulated in special ways (hence this class)
 * and treated as any AST node (hence we extend ASTNode).
 *
 * @param args A list of type arguments (e.g. class parameters)
 * @author sccblom, whmoortwijn
 */
abstract class Type(val args: List[ASTNode]) extends ASTNode {
  def this(args: Array[ASTNode]) = this(args.toList)

  def this() = this(List())

  /** Provides a Java wrapper (as `java.util.List`) for the argument list. */
  def argsJava = args.asJava

  /** Tests whether `args` has any arguments. */
  def hasArguments = args.nonEmpty

  /** Tests whether `args` has at least `n` arguments. Beware, `hasArguments` takes linear time in `n`. */
  def hasArguments(n: Int) = args.take(n).length == n

  /** Yields the first argument in `args`. */
  def firstarg = args(0)

  /** Yields the second argument in `args`. */
  def secondarg = args(1)

  /** Yields the number of arguments. But beware, `nrOfArguments` takes linear time. */
  def nrOfArguments = args.length

  def equalSize(t: Type) = false

  def isPrimitive(fraction: PrimitiveSort) = false

  def isFraction = false

  def isBoolean = false

  def isInteger = false

  def isDouble = false

  def isVoid = false

  def isNull = false

  def isIntegerType = false

  def isResource = false

  def zero: ASTNode = {
    Abort(s"zero is unimplemented for ${getClass()}")
    null
  }

  final def apply[T](map: TypeMapping[T]): T = {
    map.pre_map(this)
    var result = accept_simple(map)
    map.post_map(this, result)
  }

  def comparableWith(context: ProgramUnit, t: Type) = {
    if (isNumeric) t.isNumeric
    else equals(t) || supertypeof(context, t) || t.supertypeof(context, this)
  }

  def isNumeric = false

  /**
   * Yields a string "<`a_1_..._a_n`>" of the list `args` of arguments `a_i`,
   * provided that `args` has at least one element.
   */
  override def toString = args.isEmpty match {
    case false => s"<$argsUnderscoreSeparated>"
    case true => ""
  }

  /** Yields a comma-separated string of type arguments */
  def argsUnderscoreSeparated = args mkString "_"

  def supertypeof(unit: ProgramUnit, t: Type): Boolean

  def supertypeof(other: Type, source: Option[ProgramUnit], loader: Option[ExternalClassLoader], ns: Option[NameSpace]): Boolean =
    supertypeof(source.orNull, other)

  protected def accept_simple[T](map: TypeMapping[T]): T
}
