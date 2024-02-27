package vct.rewrite.runtime

import hre.util.ScopedStack
import vct.col.ast.RewriteHelpers._
import vct.col.ast._
import vct.col.origin.Origin
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.col.util.AstBuildHelpers._
import vct.result.VerificationError.Unreachable
import vct.rewrite.runtime.util.permissionTransfer.PermissionData
import vct.rewrite.runtime.util.LedgerHelper._

import scala.collection.mutable

object CheckPermissionsBlocksMethod extends RewriterBuilder {
  override def key: String = "CheckPermissionsBlocksMethod"

  override def desc: String = "Creates internal method blocks. In these blocks permissions will be checked"
}


case class CheckPermissionsBlocksMethod[Pre <: Generation]() extends Rewriter[Pre] {


  val isTarget: ScopedStack[Boolean] = ScopedStack()
  private val dereferences: ScopedStack[mutable.HashMap[Expr[Pre], Boolean]] = ScopedStack()
  var heapChange: Boolean = false

  implicit var program: Program[Pre] = _
  implicit var ledger: LedgerMethodBuilderHelper[Post] = _


  /**
   * Basic part of rewriting the program to retrieve ledger helper and rewrite the other declarations
   * @param program
   * @return
   */
  override def dispatch(program: Program[Pre]): Program[Rewritten[Pre]] = {
    this.program = program
    lazy val newDecl: Seq[GlobalDeclaration[Post]] = globalDeclarations.collect {
      val (ledgerHelper, _, otherDeclarations) = LedgerRewriter[Pre](this).rewriteLedger(program)
      ledger = ledgerHelper
      isTarget.having(false) {
        otherDeclarations.foreach(dispatch)
      }
    }._1
    program.rewrite(declarations = newDecl)
  }


  /**
   * If the statement is an assignment keep track of the target and if it is not a primitive type set the heapChange to true
   * If there is scope with a block in it , change the block structure of the block
   * if it is an unfold/fold create a new dereferences map so that there are no permission checks in the contract
   * @param stat
   * @return
   */
  override def dispatch(stat: Statement[Pre]): Statement[Rewritten[Pre]] = {
    stat match {
      case a: Assign[Pre] => a.rewrite(target = isTarget.having(true) {
        if(!a.target.t.isInstanceOf[PrimitiveType[Pre]]){
          heapChange = true
        }
        dispatch(a.target)
      })
      case s@Scope(_, b@Block(_)) => s.rewrite(body = determineNewBlockStructure(b))
      case _: Unfold[Pre] | _: Fold[Pre] => dereferences.collect(super.dispatch(stat))._2
      case _ => super.dispatch(stat)
    }
  }

  /**
   * When a dereference is found check if it is a target and if it is not already in the dereferences map
   * when there is a change in the heap set the value of heapChange to true
   * @param e
   * @return
   */
  override def dispatch(e: Expr[Pre]): Expr[Rewritten[Pre]] = {
    e match {
      case mi: MethodInvocation[Pre] if mi.o.getLedgerClassRuntime.nonEmpty => {
        heapChange = true
        dereferences.collect(super.dispatch(mi))._2
      }
      case inv@(_: MethodInvocation[Pre] | _: ProcedureInvocation[Pre]) => heapChange = true; super.dispatch(inv)
      case preExpr: PreAssignExpression[Pre] => preExpr.rewrite(isTarget.having(true) {
        if(preExpr.target.t.isInstanceOf[PrimitiveType[Pre]]){
          heapChange = true
        }
        dispatch(preExpr.target)
      })
      case postExpr: PostAssignExpression[Pre] => postExpr.rewrite(isTarget.having(true) {
        if(postExpr.target.t.isInstanceOf[PrimitiveType[Pre]]){
          heapChange = true
        }
        dispatch(postExpr.target)
      })
      case d@(_: Deref[Pre] | _: AmbiguousSubscript[Pre]) => {
        val newD = isTarget.having(false){ super.dispatch(d) }
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

  /**
   * Create new statements for the permission checks using the found dereferences
   * @param l
   * @param write
   * @return
   */
  private def generatePermissionChecksStatements(l: Expr[Pre], write: Boolean): Statement[Post] = {
    implicit val origin: Origin = l.o

    val location: Expr[Post] = l match {
      case d: Deref[Pre] => {
        val newDataObject: MethodInvocation[Post] = ledger.pmbh.miCreate(
          CreateObjectArray[Post](
            Seq(dispatch(d.obj),
            dispatch(const[Pre](findNumberInstanceField(program, d.ref.decl).get)))
          )).get
        ledger.miGetPermission(newDataObject).get
      }
      case AmbiguousSubscript(coll, index) => {
        val newDataObject: MethodInvocation[Post] = ledger.pmbh.miCreate(
          CreateObjectArray[Post](Seq(dispatch(coll), dispatch(index)))).get
        ledger.miGetPermission(newDataObject).get
      }
      case _ => throw Unreachable(s"This location type is not supported yet: ${l}")
    }
    val linenum = if (l.o.getStartEndLines.nonEmpty) l.o.getStartEndLines.get.startEndLineIdx._1 + 1 else -1
    val lineDetails: String = if (l.o.getStartEndLines.nonEmpty) l.o.getReadable.get.readable.readLines()(linenum - 1).trim() else "unknown line"
    val check = if (write) (location r_<=> RuntimeFractionOne[Post]()) === const(0) else (location r_<=> RuntimeFractionZero[Post]()) === const(1)
    val message = if (write) s"Permission should have been write but was not: ${l.toString}, line: ${linenum}\\n${lineDetails}" else s"Permission should have been read but there was no permission: ${l.toString}, line: ${linenum}\\n ${lineDetails}"
    RuntimeAssert[Post](check, message)(null)
  }

  /**
   * Dispatch the loop and create new block structure for the loops body
   * @param loop
   * @return
   */
  private def dispatchLoop(loop: Loop[Pre]): Loop[Post] = {
    lazy val newBody = dereferences.collect(determineNewBlockStructure(loop.body.asInstanceOf[Block[Pre]]))._2
    val contract = dereferences.collect(dispatch(loop.contract))._2 //Any dereference in the contract should not be checked by the permission checker so putting it in its own scope
    loop.rewrite(init = dispatch(loop.init), cond = dispatch(loop.cond), update = dispatch(loop.update), body = newBody, contract = contract)
  }

  /**
   * Dispatch the synchronized block and create new block structure for the synchronized blocks body
   * @param s
   * @return
   */
  private def dispatchSynchronized(s: Synchronized[Pre]): Synchronized[Post] = {
    lazy val newBody = dereferences.collect(determineNewBlockStructure(s.body.asInstanceOf[Block[Pre]]))._2
    s.rewrite(body = newBody)
  }

  /**
   * Dispatch the branch and create new block structure for the branches body
   * @param branch
   * @return
   */
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

  /**
   * retrieves the top dereferences from the dereferences stack and clears the buffer
   * @return
   */
  private def retrieveDereferences: Seq[Statement[Post]] = {
    val newAssertions: Seq[Statement[Post]] = dereferences.top.map(pair => generatePermissionChecksStatements(pair._1, pair._2)).toSeq
    dereferences.top.clear()
    newAssertions
  }

  /**
   * normal method to fold the statements of a block
   * @param b
   * @param blockFold
   * @param statement
   * @param origin
   * @return
   */
  private def defaultStatementNewMethodStructure(b: Block[Pre], blockFold: (Seq[Block[Post]], Block[Post]), statement: Statement[Pre])(implicit origin: Origin): (Seq[Block[Post]], Block[Post]) = {
    val newStatement = dispatch(statement)
    if (heapChange) {
      heapChange = false
      (blockFold._1 :+ Block[Post](retrieveDereferences ++ blockFold._2.statements) :+ Block[Post](Seq(newStatement)), Block[Post](Nil))
    } else {
      (blockFold._1, Block[Post](blockFold._2.statements :+ newStatement))
    }
  }

  /**
   * Wrapper method to also fold the statements of other block types, like loops, branches and synchronized blocks
   * @param preBlock
   * @param blockFold
   * @param statement
   * @param dispatchFunc
   * @param origin
   * @tparam T
   * @return
   */
  private def dispatchStatementWrapper[T[Pre] <: Statement[Pre]](preBlock: Block[Pre], blockFold: (Seq[Block[Post]], Block[Post]), statement: T[Pre], dispatchFunc: T[Pre] => T[Post])(implicit origin: Origin): (Seq[Block[Post]], Block[Post]) = {
    val newStat = dispatchFunc(statement)
    (blockFold._1 :+ Block[Post](retrieveDereferences ++ blockFold._2.statements :+ newStat), Block[Post](Nil))
  }

  /**
   * Method that transforms a block to a new block structure and uses the defaultStatementNewMethodStructure to fold the statements
   * If it is not a default statement it will use the dispatchStatementWrapper to fold the statements
   * @param b
   * @return
   */
  private def determineNewBlockStructure(b: Block[Pre]): Block[Post] = {
    implicit val origin: Origin = b.o
    dereferences.collect {
      val newBlocks = b.statements.foldLeft((Seq.empty[Block[Post]], Block[Post](Nil))) {
        case (blockFold, branch: Branch[Pre]) => dispatchStatementWrapper(b, blockFold, branch, dispatchBranch)
        case (blockFold, loop: Loop[Pre]) => dispatchStatementWrapper(b, blockFold, loop, dispatchLoop)
        case (blockFold, sync: Synchronized[Pre]) => dispatchStatementWrapper(b, blockFold, sync, dispatchSynchronized)
        case (blockFold, statement) => defaultStatementNewMethodStructure(b, blockFold, statement)
      }
      heapChange = false
      val finalBlock: Block[Post] = Block[Post](retrieveDereferences ++ newBlocks._2.statements)
      Block[Post](newBlocks._1 :+ finalBlock)
    }._2
  }
}