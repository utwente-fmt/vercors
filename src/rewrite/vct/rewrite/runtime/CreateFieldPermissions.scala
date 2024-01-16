package vct.rewrite.runtime

import hre.util.ScopedStack
import vct.col.ast.{Scope, _}
import vct.col.ast.RewriteHelpers._
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.rewrite.runtime.util.CodeStringDefaults._
import vct.rewrite.runtime.util.{CodeStringDefaults, FieldNumber}

import scala.collection.mutable.ArrayBuffer

object CreateFieldPermissions extends RewriterBuilder {
  override def key: String = "createFieldPermissions"

  override def desc: String = "Create permissions look up for fields and arrays and also initialize the lookup for array fields whenever this is necessary"
}


case class CreateFieldPermissions[Pre <: Generation]() extends Rewriter[Pre] {

  val methodStatements: ScopedStack[ArrayBuffer[Statement[Post]]] = ScopedStack();
  val dereferences: ScopedStack[ArrayBuffer[InstanceField[Pre]]] = ScopedStack();

  implicit var program: Program[Pre] = null


  override def dispatch(program: Program[Pre]): Program[Rewritten[Pre]] = {
    this.program = program
    val test = super.dispatch(program)
    test

  }

  private def dispatchClassDeclarations(cls: Class[Pre]): Seq[ClassDeclaration[Post]] = {
    classDeclarations.collect {
      val numberOfInstanceFields = cls.declarations.collect { case i: InstanceField[Pre] => i }.size
      if (numberOfInstanceFields >= 1) {
        val ledger = CodeStringClass[Post](newFieldPermissions(generateHashMapCreation(numberOfInstanceFields)), cls.o.getPreferredNameOrElse())(cls.o)
        classDeclarations.declare(ledger)
      }
      cls.declarations.foreach(d => dispatch(d))
      classDeclarations.declare(CodeStringClass[Post](generatedHashMapsFunction, cls.o.getPreferredNameOrElse())(cls.o))
    }._1
  }

  private def createPermissionFieldArray(instanceField: InstanceField[Pre]): Unit = {
    val id = FieldNumber(instanceField)
    val preferredName = instanceField.o.getPreferredNameOrElse()
    val cs = new CodeStringClass[Post](newArrayPermission(id), preferredName)(instanceField.o)
    classDeclarations.declare(cs)
    super.rewriteDefault(instanceField)
  }


  override def dispatch(stat: Statement[Pre]): Statement[Rewritten[Pre]] = {
    val newStatement = dereferences.having(new ArrayBuffer[InstanceField[Pre]]()) {
      super.dispatch(stat)
    }
    if (methodStatements.nonEmpty) {
      methodStatements.top.addOne(newStatement)
    }
    newStatement
  }

  private def dispatchCheckInstanceField(instanceField: InstanceField[Pre]): Unit = {
    instanceField.t match {
      case _: TArray[Pre] => createPermissionFieldArray(instanceField)
      case _ => super.rewriteDefault(instanceField)
    }
  }


  def dispatchNewArray(newArray: NewArray[Pre]): Expr[Post] = {
    if (dereferences.top.nonEmpty && methodStatements.nonEmpty) {
      val id = FieldNumber(dereferences.top(0))
      val callGHS = callGenerateHashMaps(newArray.dims.head.toString, id)
      val newStat = CodeStringStatement[Post](callGHS)(newArray.o)
      methodStatements.top.addOne(newStat)
    }
    super.dispatch(newArray)
  }


  override def dispatch(e: Expr[Pre]): Expr[Rewritten[Pre]] = {
    e match {
      case newArray: NewArray[Pre] => dispatchNewArray(newArray)
      case deref: Deref[Pre] => {
        deref.ref.decl match {
          case instanceField: InstanceField[Pre] => {
            if (dereferences.nonEmpty) {
              dereferences.top.addOne(instanceField)
            }
          }
          case _ =>
        }
        deref.rewrite()
      }
      case _ => super.dispatch(e)
    }
  }


  def dispatchMethodBlock(block: Block[Pre], im: InstanceMethod[Pre]): Block[Post] = {
    methodStatements.collect {
      super.dispatch(block) //rewriting it first to determine all the new statements
      Block[Post](methodStatements.top.toSeq)(block.o) //collecting also newly created statements in the correct order
    }._2
  }

  def dispatchInstanceMethod(im: InstanceMethod[Pre]): Unit = {
    im.body match {
      case Some(sc: Scope[Pre]) => sc.body match {
        case block: Block[Pre] => classDeclarations.succeed(im, im.rewrite(body = Some(sc.rewrite(body = dispatchMethodBlock(block, im)))))
        case _ => ???
      }
      case _ => super.rewriteDefault(im)
    }
  }

  override def dispatch(decl: Declaration[Pre]): Unit = {
    decl match {
      case cls: Class[Pre] => globalDeclarations.succeed(cls, cls.rewrite(declarations = dispatchClassDeclarations(cls)))
      case im: InstanceMethod[Pre] => dispatchInstanceMethod(im)
      case instanceField: InstanceField[Pre] => dispatchCheckInstanceField(instanceField)
      case _ => super.rewriteDefault(decl)
    }
  }
}