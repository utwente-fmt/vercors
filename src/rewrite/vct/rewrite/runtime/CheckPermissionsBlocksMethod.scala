package vct.rewrite.runtime

import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers._
import vct.col.ast._
import vct.col.origin.Origin
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.col.util.AstBuildHelpers._
import vct.result.VerificationError.Unreachable
import vct.rewrite.runtime.util.PermissionRewriter.permissionToRuntimeValue
import vct.rewrite.runtime.util.permissionTransfer.PermissionData
import vct.rewrite.runtime.util.LedgerHelper.{LedgerMethodBuilderHelper, LedgerRewriter, findNumberPrimitiveInstanceField}

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
  implicit var ledger: LedgerMethodBuilderHelper[Post] = _


  override def dispatch(program: Program[Pre]): Program[Rewritten[Pre]] = {
    this.program = program
    lazy val newDecl: Seq[GlobalDeclaration[Post]] = globalDeclarations.collect {
      val (ledgerHelper, ledgerClass, otherDeclarations) = LedgerRewriter[Pre](this).rewriteLedger(program)
      ledger = ledgerHelper
      isTarget.having(false) {
        otherDeclarations.foreach(dispatch)
      }
    }._1
    program.rewrite(declarations = newDecl)
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
      case preExpr: PreAssignExpression[Pre] => preExpr.rewrite(isTarget.having(true) {
        dispatch(preExpr.target)
      })
      case postExpr: PostAssignExpression[Pre] => postExpr.rewrite(isTarget.having(true) {
        dispatch(postExpr.target)
      })
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

    val location: Expr[Post] = l match {
      case d: Deref[Pre] if d.t.isInstanceOf[PrimitiveType[Pre]] => ledger.miGetPermission(dispatch(d.obj), dispatch(const[Pre](findNumberPrimitiveInstanceField(program, d.ref.decl).get))).get
      case d: Deref[Pre] => ledger.miGetPermission(dispatch(d.obj)).get
      case AmbiguousSubscript(coll, index) => ledger.miGetPermission(dispatch(coll), dispatch(index)).get
      case _ => throw Unreachable(s"This location type is not supported yet: ${l}")
    }
    val check = if (write) (location r_<=> RuntimeFractionOne[Post]()) === const(0) else (location r_<=> RuntimeFractionZero[Post]()) === const(1)
    val message = if (write) s"Permission should have been write but was not: ${l.toString}" else s"Permission should have been read but there was not enough permission: ${l.toString}"
    RuntimeAssert[Post](check, message)(null)
  }

  private def dispatchLoop(loop: Loop[Pre]): Loop[Post] = {
    lazy val newBody = dereferences.collect(determineNewBlockStructure(loop.body.asInstanceOf[Block[Pre]]))._2
    val contract = dereferences.collect(dispatch(loop.contract))._2 //Any dereference in the contract should not be checked by the permission checker so putting it in its own scope
    loop.rewrite(init = dispatch(loop.init), cond = dispatch(loop.cond), update = dispatch(loop.update), body = newBody, contract = contract)
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
      (blockFold._1 :+ Block[Post](retrieveDereferences ++ blockFold._2.statements) :+ Block[Post](Seq(newStatement)), Block[Post](Nil))
    } else {
      (blockFold._1, Block[Post](blockFold._2.statements :+ newStatement))
    }
  }

  private def dispatchStatementWrapper[T[Pre] <: Statement[Pre]](preBlock: Block[Pre], blockFold: (Seq[Block[Post]], Block[Post]), statement: T[Pre], dispatchFunc: T[Pre] => T[Post])(implicit origin: Origin): (Seq[Block[Post]], Block[Post]) = {
    val newStat = dispatchFunc(statement)
    (blockFold._1 :+ Block[Post](retrieveDereferences ++ blockFold._2.statements :+ newStat), Block[Post](Nil))
  }

  private def determineNewBlockStructure(b: Block[Pre]): Block[Post] = {
    implicit val origin: Origin = b.o
    dereferences.collect {
      val newBlocks = b.statements.foldLeft((Seq.empty[Block[Post]], Block[Post](Nil))) {
        case (blockFold, branch: Branch[Pre]) => dispatchStatementWrapper(b, blockFold, branch, dispatchBranch)
        case (blockFold, loop: Loop[Pre]) => dispatchStatementWrapper(b, blockFold, loop, dispatchLoop)
        case (blockFold, sync: Synchronized[Pre]) => dispatchStatementWrapper(b, blockFold, sync, dispatchSynchronized)
        case (blockFold, statement) => defaultStatementNewMethodStructure(b, blockFold, statement)
      }
      hasInvocation = false
      val finalBlock: Block[Post] = Block[Post](retrieveDereferences ++ newBlocks._2.statements)
      Block[Post](newBlocks._1 :+ finalBlock)
    }._2
  }
}