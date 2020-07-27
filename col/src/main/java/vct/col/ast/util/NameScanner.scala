package vct.col.ast.util

import java.util
import java.util.Objects

import vct.col.ast.expr._
import vct.col.ast.stmt.composite.BlockStatement
import vct.col.ast.stmt.decl.DeclarationStatement
import vct.col.ast.stmt.composite.ForEachLoop
import vct.col.ast.stmt.composite.LoopStatement
import vct.col.ast.stmt.composite.ParallelBlock
import vct.col.ast.`type`.Type
import NameExpressionKind._
import vct.col.ast.stmt.composite.VectorBlock
import vct.col.ast.stmt.terminal.AssignmentStatement

import scala.collection.mutable

// TODO (Bob): Refactor usage in ParallelBlockEncoder
// TODO (Bob): Refactor stuff from AbstractRewriter

class NameScanner extends RecursiveVisitor[AnyRef](null, null) {

  case class Entry(name: String, typ: Type, var writtenTo: Boolean)

  val frameStack: mutable.Stack[mutable.Set[DeclarationStatement]] = mutable.Stack()
  val freeNames: mutable.Map[String, Entry] = mutable.Map()

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

  def freeNamesJava: util.Hashtable[String, Type] = {
    val ht = new util.Hashtable[String, Type]()
    for (entry <- freeNames.mapValues(entry => entry.typ)) {
      ht.put(entry._1, entry._2)
    }
    ht
  }

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
          if (t != null && !(value.typ == t)) {
            Fail("type mismatch %s != %s", t, value.typ);
          }
        case None if !hasDecl(name) =>
          put(name, t)
        case _ =>
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
    // are free names.
    auto_before_after = false
  }
}