package vct.col.rewrite

import scala.collection.JavaConverters._
import vct.col.ast.stmt.decl.{ASTClass, DeclarationStatement, Method, ProgramUnit}
import vct.col.ast.util.{AbstractRewriter, AbstractVisitor}
import vct.col.ast.expr.{Dereference, MethodInvokation}
import hre.lang.System.Output
import vct.col.util.{AbstractTypeCheck, SimpleTypeCheck}

import scala.collection.mutable

class AbstractMethods(source: ProgramUnit, keepMethods: Set[String]) extends AbstractRewriter(source) {
  override def visit(m: Method): Unit = {
    if (m.getKind == Method.Kind.Plain && !keepMethods.contains(m.getName)) {
      Output("Abstracting method: %s", m.getName)
      m.setBody(null)
    }
    super.visit(m)
  }
}

object RemoveUnused {
  def minimise(pu: ProgramUnit, rootEntities: Entities): (ProgramUnit, Boolean) = {
    val entities = CollectUsed.collectUsed(pu)
    val keep = rootEntities ++ entities
    val removeMethods = new RemoveUnused(pu, keep)
    val newPu = removeMethods.rewriteAll()
    new SimpleTypeCheck(null, newPu).check()
    (newPu, removeMethods.madeChange)
  }
}
class RemoveUnused(source: ProgramUnit, keep: Entities) extends AbstractRewriter(source) {
  var madeChange = false

  override def visit(m: Method): Unit = {
    if (m.getKind == Method.Kind.Plain && !keep.methods.contains(m.getName)) {
      Output("Removing method: %s", m.getName)
      result = null
      madeChange = true
    } else if (m.getKind == Method.Kind.Pure && !keep.functions.contains(m.getName)) {
      Output("Removing function: %s", m.getName)
      result = null
      madeChange = true
    } else if (m.getKind == Method.Kind.Predicate && !keep.predicates.contains(m.getName)) {
      Output("Removing predicate: %s", m.getName)
      result = null
      madeChange = true
    } else {
      super.visit(m)
    }
  }

  override def visit(d: DeclarationStatement): Unit = {
    if (currentTargetClass == null || currentTargetClass.getName != "Ref") {
      super.visit(d);
      return
    }
    if (!(keep.fields contains d.name)) {
      Output("Removing field: %s", d.name)
      result = null
      madeChange = true
    } else {
      super.visit(d)
    }
  }
}

object CollectUsed {
  def collectUsed(pu: ProgramUnit): Entities = new CollectUsed(pu).getEntities
}

class CollectUsed(source: ProgramUnit) extends AbstractRewriter(source) {
  var total: Entities = Entities(Set(), Set(), Set(), Set())

  val methods = mutable.Set[String]()
  val functions = mutable.Set[String]()
  val predicates = mutable.Set[String]()
  val fields = mutable.Set[String]()

  def getEntities: Entities = total

  def getCurrentEntities: Entities = Entities(methods.toSet, functions.toSet, predicates.toSet, fields.toSet)

  private def clear() = {
    methods.clear()
    functions.clear()
    predicates.clear()
    fields.clear()
  }

  override def visit(m: Method): Unit = {
    total ++= getCurrentEntities
    clear()

    super.visit(m)

    if (m.getKind == Method.Kind.Plain) {
      methods -= m.getName
    } else if (m.getKind == Method.Kind.Pure) {
      functions -= m.getName
    } else if (m.getKind == Method.Kind.Predicate) {
      predicates -= m.getName
    }
  }

  override def visit(m: MethodInvokation): Unit = {
    if (m.getDefinition.getKind == Method.Kind.Plain) {
      methods += m.method
    } else if (m.getDefinition.getKind == Method.Kind.Pure) {
      functions += m.method
    } else if (m.getDefinition.getKind == Method.Kind.Predicate) {
      predicates += m.method
    }
    super.visit(m)
  }

  override def visit(d: Dereference): Unit = {
    fields += d.field
    super.visit(d)
  }

  rewriteAll()
}

case class Entities(methods: Set[String], functions: Set[String], predicates: Set[String], fields: Set[String]) {
  def ++(other: Entities): Entities = {
    Entities(methods ++ other.methods,
      functions ++ other.functions,
      predicates ++ other.predicates,
      fields ++ other.fields)
  }
}

object MinimiseSilver {
}

class MinimiseSilver(source: ProgramUnit, retainMethods: Set[String], retainFunctions: Set[String]) {
  def this(source: ProgramUnit, retainMethods: java.util.Set[String], retainFunctions: java.util.Set[String]) =
    this(source, retainMethods.asScala.toSet, retainFunctions.asScala.toSet)


  def minimise(): ProgramUnit = {

    val rootEntities = Entities(retainMethods, retainFunctions, Set(), Set())
    Output("Must retain: %s", rootEntities)

    new AbstractMethods(source, retainMethods).rewriteAll()

    var madeChange = true
    var program = source

    while (madeChange) {
      Output("-- Iter --")
      val (program1, madeChangeRemoveMethods) = RemoveUnused.minimise(program, rootEntities)

      program = program1
      madeChange = madeChangeRemoveMethods
    }

    program
  }
}
