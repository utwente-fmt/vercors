package vct.rewrite.runtime

import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers._
import vct.col.ast.{AccountedPredicate, Block, Branch, Class, Declaration, InstanceMethod, Loop, Program, Return, RewriteHelpers, Scope, Statement, UnitAccountedPredicate}
import vct.col.origin.Origin
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.rewrite.runtime.util.RewriteContractExpr
import vct.rewrite.runtime.util.permissionTransfer.PermissionData

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

  def dispatchInstanceMethod(im: InstanceMethod[Pre]): Unit = {
    im.body match {
      case Some(sc: Scope[Pre]) => sc.body match {
        case block: Block[Pre] => classDeclarations.succeed(im, im.rewrite(body = Some(sc.rewrite(body = dispatchMethodBlock(block, im)))))
        case _ => ???
      }
      case _ => super.dispatch(im)
    }
  }

  def dispatchMethodBlock(block: Block[Pre], im: InstanceMethod[Pre]): Block[Post] = {
    val preConditionStatements = dispatchApplicableContractToAssert(im.contract.requires)
    val postConditionStatements: () => Statement[Post] = () => dispatchApplicableContractToAssert(im.contract.ensures)
    val originalStatements: Seq[Statement[Post]] = block.statements.map(dispatch)
    val lastStatement: Option[Statement[Post]] = originalStatements.lastOption
    implicit val origin: Origin = im.contract.ensures.o
    val insertedPostConditions: Statement[Post] = addPostConditions(postConditionStatements, originalStatements)
    lastStatement match {
      case Some(_: Return[Post]) => Block[Post](Seq(preConditionStatements, insertedPostConditions))(block.o)
      case _ => Block[Post](Seq(preConditionStatements, insertedPostConditions, postConditionStatements()))(block.o)
    }
  }

  private def dispatchApplicableContractToAssert(ap: AccountedPredicate[Pre]): Statement[Post] = {
    ap match {
      case uni: UnitAccountedPredicate[Pre] => {
        currentContract.having(ap) {
          val pd = PermissionData().setOuter(this).setCls(currentClass.top)
          val stat = new RewriteContractExpr[Pre](pd)(program).createAssertions(uni.pred)
          dispatch(ap)
          stat
        }
      }
      case _ => ???
    }

  }

  private def addPostConditions(postConditionStatement: () => Statement[Post], originalStatements: Seq[Statement[Post]])(implicit origin: Origin): Block[Post] = {
    val newStatements = originalStatements.foldLeft[Seq[Statement[Post]]](Seq.empty[Statement[Post]]) {
      case (statements: Seq[Statement[Post]], currentStatement: Return[Post]) => statements :+ postConditionStatement() :+ currentStatement
      case (statements: Seq[Statement[Post]], loop: Loop[Post]) => statements :+ addPostConditionsLoop(postConditionStatement, loop)
      case (statements: Seq[Statement[Post]], branch: Branch[Post]) => statements :+ addPostConditionsBranch(postConditionStatement, branch)
      case (statements: Seq[Statement[Post]], currentStatement: Statement[Post]) => statements :+ currentStatement
    }
    Block[Post](newStatements)
  }

  private def addPostConditionsLoop(postConditionStatements: () => Statement[Post], loop: Loop[Post])(implicit origin: Origin): Loop[Post] = {
    loop.body match {
      case block: Block[Post] => {
        val newBlock = addPostConditions(postConditionStatements, block.statements)
        Loop[Post](loop.init, loop.cond, loop.update, loop.contract, newBlock)(loop.o)
      }
      case _ => ???
    }
  }

  private def addPostConditionsBranch(postConditionStatements: () => Statement[Post], branch: Branch[Post])(implicit origin: Origin): Branch[Post] = {
    val updatedBranches = branch.branches.map(b => {
      b._2 match {
        case block: Block[Post] => (b._1, addPostConditions(postConditionStatements, block.statements))
        case _ => b
      }
    })
    Branch[Post](updatedBranches)(branch.o)
  }
}

