package vct.rewrite.runtime

import vct.col.ast.{AccountedPredicate, AmbiguousLocation, ApplicableContract, Block, Branch, Class, CodeStringStatement, Declaration, Deref, Div, Expr, InstanceField, InstanceMethod, IntegerValue, Local, Loop, Perm, Program, ReadPerm, Result, Return, RewriteHelpers, Scope, Statement, TClass, ThisObject, UnitAccountedPredicate, WritePerm}
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import RewriteHelpers._
import hre.util.ScopedStack
import vct.result.VerificationError.Unreachable
import vct.rewrite.runtime.util.CodeStringDefaults._
import vct.rewrite.runtime.util.{FieldNumber, FieldObjectString, RewriteContractExpr}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer


object CreatePrePostConditions extends RewriterBuilder {
  override def key: String = "CreatePrePostConditions"

  override def desc: String = "Create permissions for the pre and post conditions of the methods"
}


case class CreatePrePostConditions[Pre <: Generation]() extends Rewriter[Pre] {


  val givenStatementBuffer: mutable.Buffer[Statement[Rewritten[Pre]]] = new ArrayBuffer()
  val currentClass: ScopedStack[Class[Pre]] = new ScopedStack()
  val currentContract: ScopedStack[AccountedPredicate[Pre]] = new ScopedStack()

  implicit var program: Program[Pre] = null


  override def dispatch(program: Program[Pre]): Program[Rewritten[Pre]] = {
    this.program = program
    val test = super.dispatch(program)
    test
  }

  override def dispatch(decl: Declaration[Pre]): Unit = {
    decl match {
      case im: InstanceMethod[Pre] => dispatchInstanceMethod(im)
      case cls: Class[Pre] => currentClass.having(cls) {
        globalDeclarations.succeed(cls, cls.rewrite())
      }
      case _ => super.dispatch(decl)
    }
  }

  private def dispatchApplicableContractToAssert(ap: AccountedPredicate[Pre]): Seq[Statement[Post]] = {
    ap match {
      case uni: UnitAccountedPredicate[Pre] => {
        currentContract.having(ap) {
          val (_, newStatements) = new RewriteContractExpr[Pre](this, currentClass.top)(program)
            .createStatements(uni.pred)
          dispatch(ap)
          newStatements.toSeq
        }
      }
      case _ => ???
    }

  }

  def dispatchMethodBlock(block: Block[Pre], im: InstanceMethod[Pre]): Block[Post] = {
    val preConditionStatements = dispatchApplicableContractToAssert(im.contract.requires)
    val postConditionStatements = dispatchApplicableContractToAssert(im.contract.ensures)
    val originalStatements: Seq[Statement[Post]] = block.statements.map(dispatch)
    val lastStatement: Option[Statement[Post]] = originalStatements.lastOption
    val insertedPostConditions = addPostConditions(postConditionStatements, originalStatements)
    lastStatement match {
      case Some(_: Return[Post]) => Block[Post](preConditionStatements ++ insertedPostConditions)(block.o)
      case _ => Block[Post](preConditionStatements ++ insertedPostConditions ++ postConditionStatements)(block.o)
    }
  }


  def dispatchInstanceMethod(im: InstanceMethod[Pre]): Unit = {
    im.body match {
      case Some(sc: Scope[Pre]) => sc.body match {
        case block: Block[Pre] => classDeclarations.succeed(im, im.rewrite(body = Some(sc.rewrite(body = dispatchMethodBlock(block, im)))))
        case _ => ???
      }
      case _ => super.dispatch(im)
    }
  }

  private def addPostConditions(postConditionStatements: Seq[Statement[Post]], originalStatements: Seq[Statement[Post]]): Seq[Statement[Post]] = {
    originalStatements.foldLeft[Seq[Statement[Post]]](Seq.empty[Statement[Post]]) {
      case (statements: Seq[Statement[Post]], currentStatement: Return[Post]) => statements ++ postConditionStatements :+ currentStatement
      case (statements: Seq[Statement[Post]], loop: Loop[Post]) => statements :+ addPostConditionsLoop(postConditionStatements, loop)
      case (statements: Seq[Statement[Post]], branch: Branch[Post]) => statements :+ addPostConditionsBranch(postConditionStatements, branch)
      case (statements: Seq[Statement[Post]], currentStatement: Statement[Post]) => statements :+ currentStatement
    }
  }

  private def addPostConditionsLoop(postConditionStatements: Seq[Statement[Post]], loop: Loop[Post]): Loop[Post] = {
    loop.body match {
      case block: Block[Post] => {
        val newBlock = Block[Post](addPostConditions(postConditionStatements, block.statements))(block.o)
        Loop[Post](loop.init, loop.cond, loop.update, loop.contract, newBlock)(loop.o)
      }
      case _ => ???
    }
  }

  private def addPostConditionsBranch(postConditionStatements: Seq[Statement[Post]], branch: Branch[Post]): Branch[Post] = {
    val updatedBranches = branch.branches.map(b => {
      b._2 match {
        case block: Block[Post] => (b._1, Block[Post](addPostConditions(postConditionStatements, block.statements))(block.o))
        case _ => b
      }
    })
    Branch[Post](updatedBranches)(branch.o)
  }
}

