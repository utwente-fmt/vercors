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
    for (n <- nodes.asScala) {
      n.accept(scanner)
    }
    scanner.freeNamesJava
  }

  @varargs
  def freeVars(theNodes: ASTNode*): util.Map[String, Type] = freeVars(theNodes.asJava)
}

class NameScanner extends RecursiveVisitor[AnyRef](null, null) {

  case class Entry(name: String, typ: Type, var writtenTo: Boolean)

  val frameStack: mutable.Stack[mutable.Set[DeclarationStatement]] = mutable.Stack()

  // I suspect there is some external code that depends on insertion order... Therefore use LinkedHashMap
  val freeNames: mutable.Map[String, Entry] = mutable.LinkedHashMap()

  // Ensure there is at least one frame at the start
  push()

  private def hasDecl(name: String): Boolean = frameStack.exists(frame => frame.exists(p => p.name == name))

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

  private def push(): Unit = frameStack.push(mutable.Set())

  private def pop(): Unit = frameStack.pop()

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
    * @return
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

      freeNames.get(name) match {
        case Some(value) =>
          // If the name is already a free variable, the types must match
          if (t != null && !(value.typ == t)) {
            Fail("type mismatch %s != %s", t, value.typ);
          }
        case None if !hasDecl(name) =>
          // If it is new and there is no decl, declare a new free variable
          put(name, t)
        case _ =>
          // Otherwise ignore it
      }
    case Unresolved =>
      e.getName match {
        case "tcount" =>
        case "gsize" =>
        case "tid" =>
        case "gid" =>
        case "lid" =>
        case "threadIdx" =>
        case "blockIdx" =>
        case "blockDim" =>
          put(e.getName, e.getType)
      }
    case _ =>
      Abort("missing case %s %s in name scanner", e.getKind, e.getName)
  }

  override def visit(assignment: AssignmentStatement): Unit = {
    super.visit(assignment)

    assignment.location match {
      case name: NameExpression if !hasDecl(name.name) => setWrite(name.name)
      case _ =>
    }
  }

  override def visit(d: DeclarationStatement): Unit = {
    // Discovered a new declaration
    frameStack.top.add(d)

    super.visit(d)
  }

  override def visit(s: LoopStatement): Unit = {
    push()

    super.visit(s)

    pop()
  }

  override def visit(e: BindingExpression): Unit = if (e.getDeclCount == 1) {
    push()

    super.visit(e)

    pop()
  }
  else Abort("missing case in free variable detection")

  override def visit(s: ForEachLoop): Unit = {
    push()

    super.visit(s)
    if (s.get_before != null) s.get_before.accept(this)
    if (s.get_after != null) s.get_after.accept(this)
    // Ensure before/after are not scanned after pop()
    auto_before_after = false

    pop()
  }

  override def visit(pb: ParallelBlock): Unit = {
    push()

    super.visit(pb)
    // Ensure before/after are not scanned after pop()
    auto_before_after = false

    pop()
  }

  override def visit(pb: VectorBlock): Unit = {
    push()

    super.visit(pb)

    pop()
  }

  override def visit(b: BlockStatement): Unit = {
    push()

    super.visit(b)

    pop()
  }

  override def visit(e: OperatorExpression): Unit = if ((e.operator eq StandardOperator.StructDeref) || (e.operator eq StandardOperator.StructSelect)) {
    e.first.accept(this)
    // TODO: implement struct field checking; skipping dereferenced field here
  }
  else super.visit(e)

  override def visit(invokation: MethodInvokation): Unit = {
    super.visit(invokation)
    // The before/after annotations of methods contain the given/yields mappings, so don't include them as though they
    // are free names. I.e. this ensures before/after are not scanned after pop()
    auto_before_after = false
  }
}