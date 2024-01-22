package vct.rewrite.runtime

import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers._
import vct.col.ast._
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.rewrite.runtime.util.CodeStringDefaults._
import vct.rewrite.runtime.util.FieldNumber

import scala.collection.mutable.ArrayBuffer

object CreateFieldPermissions extends RewriterBuilder {
  override def key: String = "createFieldPermissions"

  override def desc: String = "Create permissions look up for fields and arrays and also initialize the lookup for array fields whenever this is necessary"
}


case class CreateFieldPermissions[Pre <: Generation]() extends Rewriter[Pre] {

  implicit var program: Program[Pre] = null
  val blockStatements: ScopedStack[ArrayBuffer[Statement[Post]]] = ScopedStack();


  override def dispatch(program: Program[Pre]): Program[Rewritten[Pre]] = {
    this.program = program
    val test = super.dispatch(program)
    test
  }


  def dispatchClass(cls: Class[Pre]): Unit = {
    val newClassDeclarations = classDeclarations.collect {
      defineLedger(cls)
      cls.declarations.foreach(d => dispatch(d))
      defineHashMapFunction(cls)
    }._1

    globalDeclarations.succeed(cls, cls.rewrite(declarations = newClassDeclarations))
  }

  def defineLedger(cls: Class[Pre]): Unit = {
    val numberOfInstanceFields = cls.declarations.collect { case i: InstanceField[Pre] => i }.size
    if (numberOfInstanceFields >= 1) {
      val ledger = CodeStringClass[Post](newFieldPermissions(generateHashMapCreation(numberOfInstanceFields)), cls.o.getPreferredNameOrElse())(cls.o)
      classDeclarations.declare(ledger)
    }
  }

  def defineHashMapFunction(cls: Class[Pre]): Unit = {
    val hasArray = cls.declarations.collect { case i: InstanceField[Pre] => i }
      .map(i => i.t)
      .collect { case ta: TArray[Pre] => ta }
      .nonEmpty

    if (hasArray) {
      classDeclarations.declare(CodeStringClass[Post](generatedHashMapsFunction, cls.o.getPreferredNameOrElse())(cls.o))
    }
  }

  def dispatchBlock(b: Block[Pre]): Block[Post] = {
    val (newStatements, res) = blockStatements.collect {
      b.statements.foreach(dispatch)
    }
    Block[Post](newStatements)(b.o)
  }

  override def dispatch(stat: Statement[Pre]): Statement[Post] = {
    val newStatement = stat match {
      case b: Block[Pre] => dispatchBlock(b)
      case _: Loop[Pre] => blockStatements.collect {
        super.dispatch(stat)
      }._2
      case _ => super.dispatch(stat)
    }
    if (blockStatements.nonEmpty) {
      blockStatements.top.addOne(newStatement)
    }
    newStatement
  }

  override def dispatch(e: Expr[Pre]): Expr[Rewritten[Pre]] = {
    e match {
      case PreAssignExpression(d: Deref[Pre], na: NewArray[Pre]) => {
        dispatchNewArray(d, na)
        super.dispatch(e)
      }
      case _ => super.dispatch(e)
    }
  }

  def dispatchNewArray(d: Deref[Pre], na: NewArray[Pre]): Unit = {
    val instanceField: InstanceField[Pre] = d.ref.decl
    val id = FieldNumber(instanceField)
    val callGHS = callGenerateHashMaps(na.dims.head.toString, id)
    val newStat = CodeStringStatement[Post](callGHS)(na.o)
    blockStatements.top.addOne(newStat)
  }

  override def dispatch(decl: Declaration[Pre]): Unit = {
    decl match {
      case cls: Class[Pre] => dispatchClass(cls)
      case instanceField: InstanceField[Pre] => dispatchCheckInstanceField(instanceField)
      case _ => super.dispatch(decl)
    }
  }

  private def dispatchCheckInstanceField(instanceField: InstanceField[Pre]): Unit = {
    instanceField.t match {
      case _: TArray[Pre] => createPermissionFieldArray(instanceField)
      case _ => super.dispatch(instanceField)
    }
  }

  private def createPermissionFieldArray(instanceField: InstanceField[Pre]): Unit = {
    val id = FieldNumber(instanceField)
    val preferredName = instanceField.o.getPreferredNameOrElse()
    val cs = new CodeStringClass[Post](newArrayPermission(id), preferredName)(instanceField.o)
    classDeclarations.declare(cs)
    super.dispatch(instanceField)
  }
}