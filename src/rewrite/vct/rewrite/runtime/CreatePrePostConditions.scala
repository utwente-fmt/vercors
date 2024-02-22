package vct.rewrite.runtime

import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers._
import vct.col.ast._
import vct.col.origin.Origin
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.rewrite.runtime.util.LedgerHelper._
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
  val postConditions: ScopedStack[() =>  Statement[Post]] = new ScopedStack()


  val injectivityMap: ScopedStack[Variable[Post]] = new ScopedStack();

  implicit var program: Program[Pre] = null
  implicit var ledger: LedgerMethodBuilderHelper[Post] = _


  override def dispatch(program: Program[Pre]): Program[Rewritten[Pre]] = {
    this.program = program
    lazy val newDecl: Seq[GlobalDeclaration[Post]] = globalDeclarations.collect {
      val (ledgerHelper, _, otherDeclarations) = LedgerRewriter[Pre](this).rewriteLedger(program)
      ledger = ledgerHelper
      otherDeclarations.foreach(dispatch)
    }._1
    program.rewrite(declarations = newDecl)
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
        case block: Block[Pre] => {
          injectivityMap.having(ledger.createNewInjectivityMap){
            val (newVariables, newBody) = variables.collectScoped{
              sc.locals.foreach(dispatch)
              variables.declare(injectivityMap.top)
              dispatchMethodBlock(block, im)
            }
            classDeclarations.succeed(im, im.rewrite(body = Some(sc.rewrite(locals = newVariables, body = newBody))))
          }
        }
        case _ => ???
      }
      case _ => super.dispatch(im)
    }
  }

  def dispatchMethodBlock(block: Block[Pre], im: InstanceMethod[Pre]): Block[Post] = {
    implicit val origin: Origin = block.o
    val preConditionStatements: Statement[Post] = dispatchApplicableContractToAssert(im.contract.requires)
    postConditions.having(() => dispatchApplicableContractToAssert(im.contract.ensures)){
      val originalStatements: Seq[Statement[Post]] = block.statements.map(dispatch)
      val lastStatement: Option[Statement[Post]] = originalStatements.lastOption
      lastStatement match {
        case Some(_: Return[Post]) => Block[Post](preConditionStatements +: originalStatements)
        case _ => Block[Post](preConditionStatements +: originalStatements :+ postConditions.top())(block.o)
      }
    }
  }

  private def dispatchApplicableContractToAssert(ap: AccountedPredicate[Pre]): Statement[Post] = {
    ap match {
      case uni: UnitAccountedPredicate[Pre] => {
        currentContract.having(ap) {
          val pd = PermissionData().setOuter(this).setCls(currentClass.top).setLedger(ledger).setInjectivityMap(injectivityMap.top)
          val stat = new RewriteContractExpr[Pre](pd)(program).createAssertions(uni.pred)
          dispatch(ap)
          stat
        }
      }
      case _ => ???
    }
  }

  override def dispatch(stat: Statement[Pre]): Statement[Rewritten[Pre]] = {
    val dispatched = super.dispatch(stat)
    stat match {
      case _:Return[Pre] if postConditions.nonEmpty => Block[Post](Seq(postConditions.top(), dispatched))(stat.o)
      case _ => dispatched
    }
  }

}

