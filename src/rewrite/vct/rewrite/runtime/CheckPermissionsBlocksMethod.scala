package vct.rewrite.runtime

import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers._
import vct.col.ast._
import vct.col.origin.Origin
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.col.util.AstBuildHelpers._
import vct.result.VerificationError.Unreachable
import vct.rewrite.runtime.util.FindPermissionLocation
import vct.rewrite.runtime.util.PermissionRewriter.permissionToRuntimeValue
import vct.rewrite.runtime.util.permissionTransfer.PermissionData

import scala.collection.mutable

object CheckPermissionsBlocksMethod extends RewriterBuilder {
  override def key: String = "CheckPermissionsBlocksMethod"

  override def desc: String = "Creates internal method blocks. In these blocks permissions will be checked"
}


case class CheckPermissionsBlocksMethod[Pre <: Generation]() extends Rewriter[Pre] {


  val isTarget: ScopedStack[Boolean] = ScopedStack()
  private val dereferences: ScopedStack[mutable.HashMap[Expr[Pre], Boolean]] = ScopedStack()
  var hasInvocation: Boolean = false

  implicit var program: Program[Pre] = _

  override def dispatch(program: Program[Pre]): Program[Rewritten[Pre]] = {
    this.program = program
    isTarget.having(false) {
      val test = super.dispatch(program)
      test
    }

  }

  override def dispatch(stat: Statement[Pre]): Statement[Rewritten[Pre]] = {
    stat match {
      case a: Assign[Pre] => a.rewrite(target = isTarget.having(true) {
        dispatch(a.target)
      })
      case s@Scope(_, b@Block(_)) => s.rewrite(body = determineNewBlockStructure(b))
      case _ => super.dispatch(stat)
    }
  }

  override def dispatch(e: Expr[Pre]): Expr[Rewritten[Pre]] = {
    e match {
      case inv@(_: MethodInvocation[Pre] | _: ProcedureInvocation[Pre]) => hasInvocation = true; super.dispatch(inv)
      case preExpr: PreAssignExpression[Pre] => preExpr.rewrite(isTarget.having(true) {dispatch(preExpr.target)})
      case postExpr: PostAssignExpression[Pre] => postExpr.rewrite(isTarget.having(true) {dispatch(postExpr.target)})
      case d@(_: Deref[Pre] | _: AmbiguousSubscript[Pre]) => {
        val newD = super.dispatch(d)
        if (dereferences.isEmpty) return newD
        if (isTarget.top) {
          dereferences.top += (d -> true)
        } else if (!dereferences.top.contains(d)) {
          dereferences.top += (d -> false)
        }
        newD
      }
      case _ => super.dispatch(e)
    }
  }

  private def generatePermissionChecksStatements(l: Expr[Pre], write: Boolean): Statement[Post] = {
    implicit val origin: Origin = l.o
    val locator = FindPermissionLocation(PermissionData().setOuter(this))
    val location: Expr[Post] = l match {
      case d: Deref[Pre] => locator.getPermission(d).get()
      case a: AmbiguousSubscript[Pre] => locator.getPermission(a).get()
      case _ => throw Unreachable("Only Deref and AmbiguousSubscript allowed")
    }
    val check = if (write) location === permissionToRuntimeValue[Post](const[Post](1)) else location > permissionToRuntimeValue[Post](const[Post](0))
    Assert[Post](check)(null)
  }

  private def dispatchLoop(loop: Loop[Pre]): Loop[Post] = {
    lazy val newBody = dereferences.collect(determineNewBlockStructure(loop.body.asInstanceOf[Block[Pre]]))._2
    loop.rewrite(init = dispatch(loop.init), cond = dispatch(loop.cond), update = dispatch(loop.update), body = newBody)
  }

  private def dispatchSynchronized(s: Synchronized[Pre]): Synchronized[Post] = {
    lazy val newBody = dereferences.collect(determineNewBlockStructure(s.body.asInstanceOf[Block[Pre]]))._2
    s.rewrite(body = newBody)
  }

  private def dispatchBranch(branch: Branch[Pre]): Branch[Post] = {
    val gatheredConditions = branch.branches.map(b => dispatch(b._1))
    val gatheredBlocks = branch.branches
      .map(b => b._2)
      .map {
        case block: Block[Pre] => dereferences.collect(determineNewBlockStructure(block))._2
        case b => super.dispatch(b)
      }
    Branch[Post](gatheredConditions.zip(gatheredBlocks))(branch.o)
  }

  private def retrieveDereferences: Seq[Statement[Post]] = {
    val newAssertions: Seq[Statement[Post]] = dereferences.top.map(pair => generatePermissionChecksStatements(pair._1, pair._2)).toSeq
    dereferences.top.clear()
    newAssertions
  }

  private def defaultStatementNewMethodStructure(b: Block[Pre], blockFold: (Seq[Block[Post]], Block[Post]), statement: Statement[Pre])(implicit origin: Origin): (Seq[Block[Post]], Block[Post]) = {
    val newStatement = dispatch(statement)
    if (hasInvocation) {
      hasInvocation = false
      (blockFold._1 :+ Block[Post](retrieveDereferences ++ blockFold._2.statements) :+ Block[Post](Seq(newStatement)), Block[Post](Seq.empty))
    } else {
      (blockFold._1, Block[Post](blockFold._2.statements :+ newStatement))
    }
  }

  private def dispatchStatementWrapper[T[Pre] <: Statement[Pre]](preBlock: Block[Pre], blockFold: (Seq[Block[Post]], Block[Post]), statement: T[Pre], dispatchFunc: T[Pre] => T[Post])(implicit origin: Origin): (Seq[Block[Post]], Block[Post]) = {
    val newStat = dispatchFunc(statement)
    (blockFold._1 :+ Block[Post](retrieveDereferences ++ blockFold._2.statements :+ newStat), Block[Post](Seq.empty))
  }

  private def determineNewBlockStructure(b: Block[Pre]): Block[Post] = {
    implicit val origin: Origin = b.o
    dereferences.collect {
      val newBlocks = b.statements.foldLeft((Seq.empty[Block[Post]], Block[Post](Seq()))) {
        case (blockFold, branch: Branch[Pre]) => dispatchStatementWrapper(b, blockFold, branch, dispatchBranch)
        case (blockFold, loop: Loop[Pre]) => dispatchStatementWrapper(b, blockFold, loop, dispatchLoop)
        case (blockFold, sync: Synchronized[Pre]) => dispatchStatementWrapper(b, blockFold, sync, dispatchSynchronized)
        case (blockFold, statement) => defaultStatementNewMethodStructure(b, blockFold, statement)
      }
      val finalBlock: Block[Post] = Block[Post](retrieveDereferences ++ newBlocks._2.statements)
      Block[Post](newBlocks._1 :+ finalBlock)
    }._2
  }
}