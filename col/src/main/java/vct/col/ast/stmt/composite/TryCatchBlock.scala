package vct.col.ast.stmt.composite

import vct.col.ast.`type`.Type
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.decl.DeclarationStatement
import vct.col.ast.util.{ASTMapping, ASTMapping1, ASTVisitor, VisitorHelper}

import scala.collection.JavaConverters._
import scala.collection.mutable.ArrayBuffer

/**
 * AST node that represents a try-catch-finally block.
 * 
 * @author sccblom, whmoortwijn
 * @param main The body of the "try" clause.
 * @param after The body of the "finally" clause.
 * @param catchClauses An (ordered) list of "catch" clauses.
 */
class TryCatchBlock(val main:BlockStatement, val after:BlockStatement, private[this] val catchClauses:ArrayBuffer[CatchClause]) extends ASTNode with VisitorHelper {
  /** Initialises a try-catch-finally block without any catch-clauses. */
  def this(main:BlockStatement, after:BlockStatement) = this(main, after, new ArrayBuffer[CatchClause]())
  
  /** Yields the catch-clauses attached to this try-catch-block as a Java iterator. */
  def catches = catchClauses.toIterable.asJava
  
  /**
   * Adds a catch clause (i.e. an exception handler) to the try-catch-block AST node,
   * for example: "{@code catch (ExceptionType e) S}", with "{@code S}" the body of the exception handler.
   *
   * @param block The body statement block of the catch clause (e.g. the handler body "{@code S}").
   */
  def addCatchClause(name: String, catchTypes: Seq[Type], block: BlockStatement) : Unit =
    catchClauses += new CatchClause(name, catchTypes, block)

  def addCatchClauseArray(name: String, catchTypes: Array[Type], block: BlockStatement): Unit =
    addCatchClause(name, catchTypes.toSeq, block)

  def addCatchClause(cc: CatchClause): Unit = catchClauses += cc

  def numCatches(): Int = catchClauses.length

  override def accept_simple[T,A](m:ASTMapping1[T,A], arg:A) = m.map(this, arg)
  override def accept_simple[T](v:ASTVisitor[T]) = handle_standard(() => v.visit(this))
  override def accept_simple[T](m:ASTMapping[T]) = handle_standard(() => m.map(this))

  override def debugTreeChildrenFields(): Iterable[String] = Seq("main", "catchClauses", "after")
  override def debugTreePropertyFields(): Iterable[String] = Seq()
}
