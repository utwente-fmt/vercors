package vct.col.ast.stmt.composite

import vct.col.ast.generic.{ASTNode, ASTSequence}
import vct.col.ast.util.{ASTMapping, ASTMapping1, ASTVisitor, VisitorHelper}

import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters._

/**
 * AST node that represents a block of statements, that is, a sequence
 * "`S_1;...;S_n`" of (individual) statements `S_i`.
 */
class BlockStatement extends ASTNode with ASTSequence[BlockStatement] with VisitorHelper {
  /** The list of statements that constitutes the statement block. */
  private[this] val statements = new ArrayBuffer[ASTNode]

  /**
   * Adds a statement `stmt` to this block of statements.
   *
   * @return The resulting statement block (after adding `stmt`).
   */
  def addStatement(stmt: ASTNode): BlockStatement = add(stmt)

  /**
   * Appends a given AST node "`node`", representing a statement, to this block of
   * statements by applying chaining (see the method `chain`).
   *
   * @return The resulting statement block (after appending `node`).
   */
  def append(node: ASTNode): BlockStatement = chain(node)

  /**
   * Adds the given AST node "`node`" to this block statement, and if `node` is
   * a block statement itself, all `node`'s (internal) statements are added instead.
   *
   * @param node The node to "chain" to the block statement
   * @return The resulting statement block (after "chaining" `node`).
   */
  def chain(node: ASTNode): BlockStatement = node match {
    case block: BlockStatement => block.forEachStmt { stmt => stmt.clearParent(); add(stmt) }; this
    case other => add(other)
  }

  /** Applies the function "`f`" to each statement in this block (works the same as the
   * `forEach` method provided by the [[Iterable]] interface, only `forEachStmt`
   * works nicer with Scala code). */
  def forEachStmt(f: ASTNode => Unit) = statements.foreach(f)

  /**
   * Adds "{@code node}" to (the back of) this statement block.
   *
   * @return The resulting statement block (after adding `node`).
   */
  override def add(node: ASTNode): BlockStatement = {
    add(Option(node))
    this
  }

  /** If `element` contains an AST node, that node is added to
   * (the back of) this statement block (otherwise nothing is added). */
  def add(element: Option[ASTNode]): Unit = element match {
    case Some(node) => node.setParent(this); statements += node
    case None => ()
  }

  /** Identical to `get(i)`. */
  def getStatement(i: Int) = get(i)

  /** Yields the `i`-th statement in this statement block. */
  override def get(i: Int): ASTNode = statements.apply(i)

  /** Yields a copy of the statements in this statement block. */
  def getStatements = statements.clone.toArray

  /** Tests whether the statement block is empty (does not contain any statements). */
  def isEmpty = statements.isEmpty

  /** Prepends the given AST node "`node`" to the front of this statement block. */
  def prepend(node: ASTNode): Unit = prepend(Option(node))

  /**
   * Prepends `element` to this statement block (i.e. `element` is added to
   * the front of the statement list).
   *
   * @note This method behaves differently from `append`, as `append` performs "chaining"
   *       (see the `chain` method) whereas `prepend` does not.
   */
  def prepend(element: Option[ASTNode]): Unit = element match {
    case Some(node) => node.setParent(this); statements.+=:(node)
    case None => ()
  }

  /** Identical to `size()`. */
  def getLength: Int = size

  /** Yields the number of statements in this statement block. */
  override def size: Int = statements.length

  /** Yields an (Java) iterator for the statements in this block. */
  override def iterator = statements.iterator.asJava

  override def accept_simple[T, A](m: ASTMapping1[T, A], arg: A) = m.map(this, arg)

  override def accept_simple[T](v: ASTVisitor[T]) = handle_standard(() => v.visit(this))

  override def accept_simple[T](m: ASTMapping[T]) = handle_standard(() => m.map(this))

  override def debugTreeChildrenFields: Iterable[String] = Seq("statements")

  override def debugTreePropertyFields: Iterable[String] = Seq()
}
