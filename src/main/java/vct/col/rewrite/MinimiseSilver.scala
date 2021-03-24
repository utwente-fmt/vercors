package vct.col.rewrite

import scala.collection.JavaConverters._
import vct.col.ast.stmt.decl.{ASTClass, DeclarationStatement, Method, ProgramUnit}
import vct.col.ast.util.{AbstractRewriter, AbstractVisitor, RecursiveVisitor}
import vct.col.ast.expr.{Dereference, MethodInvokation}
import hre.lang.System.{Output, Warning, Debug}
import vct.col.ast.`type`.ASTReserved
import vct.col.util.{AbstractTypeCheck, SimpleTypeCheck}

import scala.collection.mutable

// Still needed:
// - Axiom minimisation
//  - If functions are not used outside of adt this is trivial
//  - If some are used outside, but some not, and the unused ones do not occur in axioms of
//    the used ones, those can also be remove
// - VCTOption elision (if null is used nowhere, or only appears in != null clauses)
//  - Might have to make this one opt-in since I'm not sure we can syntactically detect if this is feasible...
// - VCTArray -> Seq would be nice, but that seems to complicated... maybe opt-in or in very specific cases?

class AbstractMethods(source: ProgramUnit, keepMethods: Set[String]) extends AbstractRewriter(source) {
  override def visit(m: Method): Unit = {
    if (m.getKind == Method.Kind.Plain && !keepMethods.contains(m.getName)) {
      Debug("Abstracting method: %s", m.getName)
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
      Debug("Removing method: %s", m.getName)
      result = null
      madeChange = true
    } else if (m.getKind == Method.Kind.Pure && !keep.functions.contains(m.getName)) {
      Debug("Removing function: %s", m.getName)
      result = null
      madeChange = true
    } else if (m.getKind == Method.Kind.Predicate && !keep.predicates.contains(m.getName)) {
      Debug("Removing predicate: %s", m.getName)
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
      Debug("Removing field: %s", d.name)
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

  private def getCurrentEntities: Entities = Entities(methods.toSet, functions.toSet, predicates.toSet, fields.toSet)

  private def clear() = {
    methods.clear()
    functions.clear()
    predicates.clear()
    fields.clear()
  }

  /**
   * Flushes the working set of encountered uses to the total set of encountered uses.
   */
  private def flush() = {
    total ++= getCurrentEntities
    clear()
  }

  override def visit(m: Method): Unit = {
    // Uses of a method by itself needs to be prevented. Therefore we make the collected uses empty now,
    // and after visiting the method, remove the appearance of the method's own name from the set.
    // That prevents recursive uses from occurring in the final use set.
    flush()

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
  // Ensure that any uses in the final method, adt, class etc. are not discarded
  // since they need to be merged into total first.
  flush()
}

object CollectRetainEntities {
  def collect(source: ProgramUnit, names: Set[String]): (Entities, Set[String]) = {
    val cre = new CollectRetainEntities(source, names)
    source.accept(cre)
    (cre.getEntities, cre.leftoverNames.toSet)
  }
}

/**
  * Collects all names in `names` that fit any of the supported categories into the Entities type.
  * Assumes all values in `names` are unique in the AST.
  */
class CollectRetainEntities(source: ProgramUnit, names: Set[String]) extends RecursiveVisitor(source) {
  val leftoverNames: mutable.Set[String] = names.to(mutable.Set)

  val methods = mutable.Set[String]()
  val functions = mutable.Set[String]()
  val predicates = mutable.Set[String]()
  val fields = mutable.Set[String]()

  var inRef = false

  private def getEntities: Entities = Entities(methods.toSet, functions.toSet, predicates.toSet, fields.toSet)

  override def visit(m: Method): Unit = {
    if ((leftoverNames contains m.name) || m.hasAnnotation(ASTReserved.MinimiseTarget)) {
      m.kind match {
        case Method.Kind.Pure =>
          functions.add(m.getName)
        case Method.Kind.Predicate =>
          predicates.add(m.getName)
        case Method.Kind.Plain =>
          methods.add(m.getName)
        case k =>
          Warning("Unexpected kind %s for name %s, ignoring", k, m.name)
      }
      leftoverNames.remove(m.name)
    }
  }

  override def visit(c: ASTClass): Unit = {
    if (c.name == "Ref") {
      inRef = true
      super.visit(c)
      inRef = false
    } else {
      super.visit(c)
    }
  }

  override def visit(d: DeclarationStatement): Unit = {
    if (inRef && (leftoverNames contains d.name)) {
      fields.add(d.name)
      leftoverNames.remove(d.name)
    }
  }
}

case class Entities(methods: Set[String], functions: Set[String], predicates: Set[String], fields: Set[String]) {
  def ++(other: Entities): Entities = {
    Entities(methods ++ other.methods,
      functions ++ other.functions,
      predicates ++ other.predicates,
      fields ++ other.fields)
  }

  def size: Int = methods.size + functions.size + predicates.size + fields.size
}

class MinimiseSilver(source: ProgramUnit, retainNames: Set[String]) {
  def this(source: ProgramUnit, retainMethods: java.util.List[String]) =
    this(source, retainMethods.asScala.toSet)

  def minimise(): ProgramUnit = {
    val silverRetainNames = retainNames.filter(!_.contains("#"))
    val (rootEntities, leftoverNames) = CollectRetainEntities.collect(source, silverRetainNames)
    if (rootEntities.size < retainNames.size) {
      Warning("Number of entities selected for retention (%s) is smaller than number of entities indicated for retention (%s)." +
        " This means the output will be oversimplified, which is a bug. Please report an issue.",
        rootEntities,
        retainNames.mkString(", ")
      )
    }
    Debug("Must retain: %s", rootEntities)
    if (leftoverNames.nonEmpty) {
      Warning("The following names were not found in the Silver AST: %s", leftoverNames.mkString(", "))
    }

    new AbstractMethods(source, rootEntities.methods).rewriteAll()

    var madeChange = true
    var program = source

    while (madeChange) {
      Debug("-- MinimiseSilver: Iter --")
      val (program1, madeChangeRemoveMethods) = RemoveUnused.minimise(program, rootEntities)

      program = program1
      madeChange = madeChangeRemoveMethods
    }

    program
  }
}
