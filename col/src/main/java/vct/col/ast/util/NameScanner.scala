package vct.col.ast.util

import vct.col.ast.`type`.Type
import vct.col.ast.expr.NameExpressionKind._
import vct.col.ast.expr._
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.composite._
import vct.col.ast.stmt.decl.{ASTSpecial, DeclarationStatement}
import vct.col.ast.stmt.terminal.AssignmentStatement

import java.util
import scala.annotation.varargs
import scala.collection.mutable
import scala.jdk.CollectionConverters._

object NameScanner {
  @varargs
  def freeVars(theNodes: ASTNode*): util.Map[String, Type] = freeVars(theNodes.asJava)

  def freeVars[R <: ASTNode](nodes: util.List[R]): util.Map[String, Type] =
    scanned(nodes.asScala).freeNamesJava

  private def scanned(nodes: Iterable[ASTNode]): NameScanner = {
    val scanner = new NameScanner
    nodes.foreach(_.accept(scanner))
    scanner
  }

  def accesses(arg: ASTNode): Set[String] =
    scanned(Seq(arg)).accesses

  def writes(arg: ASTNode): Set[String] =
    scanned(Seq(arg)).writes

  def reads(arg: ASTNode): Set[String] =
    scanned(Seq(arg)).reads

  def labels(arg: ASTNode): Set[String] =
    scanned(Seq(arg)).labels.toSet

  def labelsJava(arg: ASTNode): util.List[String] =
    scanned(Seq(arg)).labels.toSeq.asJava
}

class NameScanner extends RecursiveVisitor[AnyRef](null, null) {

  val frameStack: mutable.Stack[mutable.Set[DeclarationStatement]] = mutable.Stack()
  /**
   * Given/yields must be stacks as well, in case with/then blocks are nested
   */
  val givenStack: mutable.Stack[mutable.Set[DeclarationStatement]] = mutable.Stack()
  val yieldsStack: mutable.Stack[mutable.Set[DeclarationStatement]] = mutable.Stack()
  /**
   * There might be external code that depends on insertion order. Therefore use LinkedHashMap
   */
  val freeNames: mutable.Map[String, Entry] = mutable.LinkedHashMap()
  val labels: mutable.Set[String] = mutable.Set()
  /**
   * Here all initBlocks of LoopStatements are saved. This can be used when visiting BlockStatements to not
   * pop the frameStack, as declarations within initBlocks need to be visible in the rest of the LoopStatement.
   */
  val initBlocks: mutable.Set[BlockStatement] = mutable.Set()

  /**
   * Returns free names as a java map with insertion order retained
   */
  def freeNamesJava: util.Map[String, Type] = {
    val map = new util.LinkedHashMap[String, Type]()
    freeNames.foreach(mapEntry => map.put(mapEntry._1, mapEntry._2.typ))
    map
  }

  // Ensure there is at least one frame at the start
  pushFrame()

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
      checkName(e.getName, e.getType, getYieldsDecl)

    case Unresolved =>
      if (Seq("tcount", "gsize", "tid", "gid", "lid", "threadIdx", "blockIdx", "blockDim").contains(e.getName)) {
        checkName(e.getName, e.getType, getYieldsDecl)
      }
    case _ =>
      Abort("missing case %s %s in name scanner", e.getKind, e.getName)
  }

  private def getYieldsDecl(name: String): Option[DeclarationStatement] = yieldsStack
    .map(_.find(_.name == name))
    .collectFirst { case Some(decl) => decl }

  override def visit(assignment: AssignmentStatement): Unit = {
    assignment.location match {
      case name: NameExpression =>
        if (checkName(name.name, name.getType, getGivenDecl)) {
          setWrite(name.name)
        }
      case _ =>
    }

    assignment.expression.accept(this)
  }

  /** If the name is in `additionalNames`, or there is a regular declaration with the same name, the types must match
   * If there is a free variable with the same name, the types must match
   * If there is no decl nor a free variable, we found a new free variable, which is added to the `freeNames` list
   *
   * @param additionalNames can be used to pass in given or yields scopes.
   * @return true if `name` is the name of a free variable.
   */
  private def checkName(name: String, typ: Type, additionalNames: String => Option[DeclarationStatement]): Boolean =
    (additionalNames(name) orElse getDecl(name), freeNames.get(name)) match {
      case (Some(decl), _) =>
        if (typ != null && decl.`type` != typ) {
          Fail("type mismatch %s != %s", typ, decl.`type`)
        }
        false
      case (None, Some(entry)) =>
        if (typ != null && entry.typ != typ) {
          Fail("type mismatch %s != %s", typ, entry.typ)
        }
        true
      case _ =>
        put(name, typ)
        true
    }

  /**
   * Puts a _new_ variable name into the freeNames list
   */
  private def put(name: String, typ: Type): Unit = {
    if (hasDecl(name)) {
      Abort("Cannot put a free var when it has a decl")
    }

    freeNames.get(name) match {
      case Some(_) =>
        // Disallow putting the same free var again, to prevent the writtenTo flag to be accidentally reset
        Abort("Name %s is already marked as free", name);
      case None =>
        freeNames.put(name, Entry(name, typ, writtenTo = false));
    }
  }

  /**
   * True if there is a declaration in frameStack with the same name.
   */
  private def hasDecl(name: String): Boolean = getDecl(name).isDefined

  /**
   * Finds the first declaration in the framestack that has name `name`
   */
  private def getDecl(name: String): Option[DeclarationStatement] = frameStack
    .map(_.find(_.name == name))
    .collectFirst { case Some(decl) => decl }

  private def getGivenDecl(name: String): Option[DeclarationStatement] = givenStack
    .map(_.find(_.name == name))
    .collectFirst { case Some(decl) => decl }

  private def setWrite(name: String): Unit =
    freeNames.get(name) match {
      case Some(value) => value.writtenTo = true
      case None => Abort("Cannot set write for non-existent variable %s", name)
    }

  override def visit(d: DeclarationStatement): Unit = {
    // Discovered a new declaration
    frameStack.top.add(d)

    super.visit(d)
  }

  override def visit(s: LoopStatement): Unit = {
    pushFrame()

    initBlocks += s.getInitBlock.asInstanceOf[BlockStatement]

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

  private def pushFrame(): Unit = frameStack.push(mutable.Set())

  private def popFrame(): Unit = frameStack.pop()

  override def visit(pb: VectorBlock): Unit = {
    pushFrame()

    super.visit(pb)

    popFrame()
  }

  override def visit(b: BlockStatement): Unit = {
    // Declarations in initblocks are supposed to leak into other components of LoopStatement.
    // Therefore a frame is not pushed when an initBlock is encountered.
    val isInitBlock = initBlocks contains b
    if (!isInitBlock) pushFrame()

    super.visit(b)

    if (!isInitBlock) popFrame()
  }

  override def visit(e: OperatorExpression): Unit = if ((e.operator eq StandardOperator.StructDeref) || (e.operator eq StandardOperator.StructSelect)) {
    e.first.accept(this)
  }
  else super.visit(e)

  override def visit(invokation: MethodInvokation): Unit = {
    super.visit(invokation)

    if (invokation.getDefinition != null && invokation.getDefinition.getContract != null) {
      val contract = invokation.getDefinition.getContract

      givenStack.push(mutable.Set())
      contract.`given`.foreach(givenStack.top.add(_))
      if (invokation.get_before != null) invokation.get_before.accept(this)
      givenStack.pop()

      yieldsStack.push(mutable.Set())
      contract.yields.foreach(yieldsStack.top.add(_))
      if (invokation.get_after != null) invokation.get_after.accept(this)
      yieldsStack.pop()
    } else {
      // If the definition is null, there better not be any before/after statements, because those would
      // influence the computation of free variables.
      if (invokation.get_before.size + invokation.get_after.size != 0) {
        // If this happens, we cannot check which variables are free, because this is in part
        // determined by which given/yields variables are in the contract of the definition.
        // Therefore, we abort, since this is a bug
        Abort("Definition is null, but before/after statements are present")
      }
    }

    // This ensures before/after are not scanned after pop()
    auto_before_after = false
  }

  override def visit(special: ASTSpecial): Unit = {
    special.kind match {
      case ASTSpecial.Kind.Label =>
        labels += special.args(0).toString
      case _ =>
    }

    super.visit(special)
  }

  case class Entry(name: String, typ: Type, var writtenTo: Boolean)
}