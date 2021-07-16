package vct.col.ast.stmt.decl

import hre.lang.System.Abort
import vct.col.ast.util._

import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters._

/**
 * @author sccblom, whmoortwijn
 * @note We may refactor the three mutable buffers if we first refactor `CheckHistoryAlgebra`.
 */
case class AxiomaticDataType(override val name: String, val parameters: List[DeclarationStatement]) extends ASTDeclaration(name) with VisitorHelper {
  require(parameters != null, "The list of parameters is null")

  private val axioms = new ArrayBuffer[Axiom]()
  private val constructors = new ArrayBuffer[Method]()
  private val mappings = new ArrayBuffer[Method]()

  /** Constructs a new ADT from Java constructs */
  def this(name: String, pars: Array[DeclarationStatement]) = this(name, pars.toList)

  /** Provides a Java wrapper (as an `java.util.List`) for the list of parameters. */
  def parametersJava = parameters.asJava

  def axiomsJava = axioms.toIterable.asJava

  def constructorsJava = constructors.toIterable.asJava

  def mappingsJava = mappings.toIterable.asJava

  def add_map(m: Method): Unit = {
    m.setFlag(ASTFlags.STATIC, true)
    m.setParent(this)
    mappings += m
  }

  def add_cons(m: Method): Unit = {
    m.setFlag(ASTFlags.STATIC, true)
    m.setParent(this)
    constructors += m
  }

  def add_unique_cons(m: Method): Unit = {
    if (m.getArgs.length != 0) {
      Abort("Unique constructors cannot have any formal parameters");
    }

    m.setFlag(ASTFlags.UNIQUE, true);
    m.setFlag(ASTFlags.STATIC, true);
    m.setParent(this)
    constructors += m
  }

  def add_axiom(ax: Axiom): Unit = {
    ax.setParent(this);
    axioms += ax
  }

  override def accept_simple[T, A](m: ASTMapping1[T, A], arg: A) = m.map(this, arg)

  override def accept_simple[T](v: ASTVisitor[T]) = handle_standard(() => v.visit(this))

  override def accept_simple[T](m: ASTMapping[T]) = handle_standard(() => m.map(this))

  override def getDeclName = new ClassName(name)

  override def debugTreeChildrenFields: Iterable[String] = Seq("parameters")

  override def debugTreePropertyFields: Iterable[String] = Seq("name")
}
