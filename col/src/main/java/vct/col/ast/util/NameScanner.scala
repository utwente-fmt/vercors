package vct.col.ast.util

import java.util
import java.util.Objects

import scala.collection.JavaConverters._
import vct.col.ast.expr._
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.composite.BlockStatement
import vct.col.ast.stmt.decl.DeclarationStatement
import vct.col.ast.stmt.composite.ForEachLoop
import vct.col.ast.stmt.composite.LoopStatement
import vct.col.ast.stmt.composite.ParallelBlock
import vct.col.ast.`type`.Type
import NameExpressionKind._

import vct.col.ast.stmt.composite.VectorBlock

object NameScanner {
  def occurCheck(invariant: ASTNode, var_name: String): Boolean = {
    val vars = new util.Hashtable[String, Type]
    invariant.accept(new NameScanner(vars))
    vars.containsKey(var_name)
  }
}

class NameScanner(var vars: util.Hashtable[String, Type]) extends RecursiveVisitor[AnyRef](null, null) {
  private val safe_decls = new util.HashSet[DeclarationStatement]

  override def visit(e: NameExpression): Unit = e.getKind match {
    case Reserved =>
    case Label =>
    case Field | Local | Argument =>
      val name = e.getName
      val t = e.getType
      if (vars.containsKey(name)) {
        if (t != null && !(t == vars.get(name))) {
          Fail("type mismatch %s != %s", t, vars.get(name))
        }
      } else {
        Objects.requireNonNull(t, String.format("type of %s is null", name))
        vars.put(name, t)
        t.accept(this)
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
          vars.put(e.getName, e.getType)
      }
    case _ =>
      Abort("missing case %s %s in name scanner", e.getKind, e.getName)
  }

  override def visit(d: DeclarationStatement): Unit = {
    if (!safe_decls.contains(d)) Abort("missing case in free variable detection")
    super.visit(d)
  }

  override def visit(s: LoopStatement): Unit = {
    var init = s.getInitBlock
    init match {
      case block: BlockStatement =>
        if (block.getLength == 1) init = block.get(0)
      case decl: DeclarationStatement =>
        val old = vars.get(decl.name)
        safe_decls.add(decl)
        super.visit(s)
        vars.remove(decl.name)
        if (old != null) vars.put(decl.name, old)
      case _ => super.visit(s)
    }
  }

  override def visit(e: BindingExpression): Unit = if (e.getDeclCount == 1) {
    val decl = e.getDeclaration(0)
    val old = vars.get(decl.name)
    safe_decls.add(decl)
    super.visit(e)
    vars.remove(decl.name)
    if (old != null) vars.put(decl.name, old)
  }
  else Abort("missing case in free variable detection")

  override def visit(s: ForEachLoop): Unit = {
    val old = new Array[Type](s.decls.length)
    for (i <- s.decls.indices) {
      old(i) = vars.get(s.decls(i).name)
      safe_decls.add(s.decls(i))
    }
    super.visit(s)
    if (s.get_before != null) s.get_before.accept(this)
    if (s.get_after != null) s.get_after.accept(this)
    auto_before_after = false
    for (i <- s.decls.indices) {
      vars.remove(s.decls(i).name)
      if (old(i) != null) vars.put(s.decls(i).name, old(i))
    }
  }

  override def visit(pb: ParallelBlock): Unit = {
    val oldi = new Array[Type](pb.iterslength)
    var i = 0
    for (decl <- pb.iters) {
      oldi(i) = vars.get(decl.name)
      safe_decls.add(decl)
      i += 1
    }
    super.visit(pb)
    auto_before_after = false
    i = 0
    for (decl <- pb.iters) {
      vars.remove(decl.name)
      if (oldi(i) != null) vars.put(decl.name, oldi(i))
      i += 1
    }
  }

  override def visit(pb: VectorBlock): Unit = {
    val old = vars.get(pb.iter.name)
    safe_decls.add(pb.iter)
    super.visit(pb)
    vars.remove(pb.iter.name)
    if (old != null) vars.put(pb.iter.name, old)
  }

  override def visit(b: BlockStatement): Unit = {
    val N = b.size
    val saved_vars = new util.Hashtable[String, Type]
    val saved_names = new util.HashSet[String]
    for (i <- 0 until N) {
      val s = b.get(i)
      s match {
        case decl: DeclarationStatement =>
          val old = vars.get(decl.name)
          saved_names.add(decl.name)
          if (old != null) saved_vars.put(decl.name, old)
          safe_decls.add(decl)
        case _ =>
      }
      s.accept(this)
    }
    for (name <- saved_names.iterator().asScala) {
      vars.remove(name)
      val old = saved_vars.get(name)
      if (old != null) vars.put(name, old)
    }
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