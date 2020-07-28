package vct.col.ast.util

import java.util
import java.util.Objects
import scala.annotation.varargs
import scala.collection.mutable
import scala.collection.JavaConverters._
import vct.col.ast.expr._
import vct.col.ast.stmt.composite.BlockStatement
import vct.col.ast.stmt.decl.DeclarationStatement
import vct.col.ast.stmt.composite.ForEachLoop
import vct.col.ast.stmt.composite.LoopStatement
import vct.col.ast.stmt.composite.ParallelBlock
import vct.col.ast.`type`.Type
import NameExpressionKind._
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.composite.VectorBlock
import vct.col.ast.stmt.terminal.AssignmentStatement

object NameScanner {
  def freeVars[R <: ASTNode](nodes: util.List[R]): util.Map[String, Type] = {
    val scanner = new NameScanner
    nodes.asScala.foreach(_.accept(scanner))
    scanner.freeNamesJava
  }

  @varargs
  def freeVars(theNodes: ASTNode*): util.Map[String, Type] = freeVars(theNodes.asJava)
}

class NameScanner extends RecursiveVisitor[AnyRef](null, null) {

  case class Entry(name: String, typ: Type, var writtenTo: Boolean)

  val frameStack: mutable.Stack[mutable.Set[DeclarationStatement]] = mutable.Stack()
  val givenYieldsStack: mutable.Stack[mutable.Set[DeclarationStatement]] = mutable.Stack()

  // There might be external code that depends on insertion order. Therefore use LinkedHashMap
  val freeNames: mutable.Map[String, Entry] = mutable.LinkedHashMap()

  // Ensure there is at least one frame at the start
  pushFrame()

  /**
    * Finds the first declaration in the framestack that has name `name`
    */
  private def getDecl(name: String): Option[DeclarationStatement] = frameStack
    // Unlift turns a function returning Option into a partial function
    .collectFirst(Function.unlift(_.find(decl => decl.name == name)))

  /**
    * True if there is a regular declaration (i.e. not a given/yields declaration) in scope with the same name.
    */
  private def hasDecl(name: String): Boolean = getDecl(name).isDefined

  /**
    * True if there is a given/yields declaration of one of the currently active methods
    * with the same name.
    */
  private def hasGivenYieldsDecl(name: String): Boolean = givenYieldsStack.exists(frame => frame.exists(decl => decl.name == name))

  private def put(name: String, typ: Type): Unit = {
    if (hasDecl(name)) {
      Abort("Cannot put a free var when it has a decl")
    }

    Objects.requireNonNull(typ)

    freeNames.get(name) match {
      case Some(_) =>
        // Disallow putting the same free var again, to prevent the writtenTo flag to be accidentally reset
        Abort("Name %s is already marked as free", name);
      case None =>
        freeNames.put(name, Entry(name, typ, writtenTo = false));
    }
  }

  private def setWrite(name: String): Unit =
    freeNames.get(name) match {
      case Some(value) => value.writtenTo = true
      case None => Abort("Cannot set write for non-existent variable %s", name)
    }

  private def pushFrame(): Unit = frameStack.push(mutable.Set())

  private def popFrame(): Unit = frameStack.pop()

  private def pushGivenYields(): Unit = givenYieldsStack.push(mutable.Set())

  private def popGivenYields(): Unit = givenYieldsStack.pop()

  /**
    * Returns free names as a java map with insertion order retained
    */
  def freeNamesJava: util.Map[String, Type] = {
    val map = new util.LinkedHashMap[String, Type]()
    freeNames.foreach(mapEntry => map.put(mapEntry._1, mapEntry._2.typ))
    map
  }

  /**
    * All variable names that are read from or written to
    */
  def accesses: Set[String] = freeNames.keySet.toSet

  def writes: Set[String] = freeNames.filter(p => p._2.writtenTo).keySet.toSet

  def reads: Set[String] = freeNames.filter(p => !p._2.writtenTo).keySet.toSet

  override def visit(e: NameExpression): Unit = e.getKind match {
    case Reserved =>
    case Label =>
    case Field | Local | Argument =>
      val name = e.getName
      val t = e.getType

      // If there is a declaration with the same name, the types must match
      // If there is a free variable with the same name, the types must match
      // If there is no decl nor a free variable, we found a new free variable
      (getDecl(name), freeNames.get(name)) match {
        case (Some(decl), _) =>
          if (t != null && decl.`type` != t) {
            Fail("type mismatch %s != %s", t, decl.`type`);
          }
        case (None, Some(entry)) =>
          if (t != null && entry.typ != t) {
            Fail("type mismatch %s != %s", t, entry.typ);
          }
        case (None, None) => put(name, t)
      }

    case Unresolved =>
      e.getName match {
        case "tcount" | "gsize" | "tid" | "gid" | "lid" | "threadIdx" | "blockIdx" | "blockDim" =>
          put(e.getName, e.getType)
        case _ =>
      }
    case _ =>
      Abort("missing case %s %s in name scanner", e.getKind, e.getName)
  }

  override def visit(assignment: AssignmentStatement): Unit = {
    super.visit(assignment)

    // If a name has no regular decl, nor a given/yields decl, it is a free variable
    assignment.location match {
      case name: NameExpression if !hasDecl(name.name) && !hasGivenYieldsDecl(name.name) => setWrite(name.name)
      case _ =>
    }
  }

  override def visit(d: DeclarationStatement): Unit = {
    // Discovered a new declaration
    frameStack.top.add(d)

    super.visit(d)
  }

  override def visit(s: LoopStatement): Unit = {
    pushFrame()

    super.visit(s)

    popFrame()
  }

  override def visit(e: BindingExpression): Unit = {
    pushFrame()

    super.visit(e)

    popFrame()
  }

  override def visit(s: ForEachLoop): Unit = {
    pushFrame()

    super.visit(s)
    if (s.get_before != null) s.get_before.accept(this)
    if (s.get_after != null) s.get_after.accept(this)
    // Ensure before/after are not scanned after pop()
    auto_before_after = false

    popFrame()
  }

  override def visit(pb: ParallelBlock): Unit = {
    pushFrame()

    super.visit(pb)

    popFrame()
  }

  override def visit(pb: VectorBlock): Unit = {
    pushFrame()

    super.visit(pb)

    popFrame()
  }

  override def visit(b: BlockStatement): Unit = {
    pushFrame()

    super.visit(b)

    popFrame()
  }

  override def visit(e: OperatorExpression): Unit = if ((e.operator eq StandardOperator.StructDeref) || (e.operator eq StandardOperator.StructSelect)) {
    e.first.accept(this)
    // TODO: implement struct field checking; skipping dereferenced field here
  }
  else super.visit(e)

  override def visit(invokation: MethodInvokation): Unit = {
    super.visit(invokation)

    pushGivenYields()

    val contract = invokation.getDefinition.getContract()
    contract.`given`.foreach(givenYieldsStack.top.add(_))
    contract.yields.foreach(givenYieldsStack.top.add(_))

    if (invokation.get_before != null) invokation.get_before.accept(this)
    if (invokation.get_after != null) invokation.get_after.accept(this)

    // This ensures before/after are not scanned after pop()
    auto_before_after = false

    popGivenYields()
  }
}