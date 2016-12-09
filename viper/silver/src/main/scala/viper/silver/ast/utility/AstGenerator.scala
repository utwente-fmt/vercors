/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package viper.silver.ast.utility

import viper.silver.ast._
import viper.silver.utility.SilNameGenerator

object AstGenerator {

  def toAst(block: Block) = AstGeneratorContext(block).toAst

  case class AstGeneratorContext(block: Block) {

    private val nameGen = new SilNameGenerator()

    lazy val surroundingLoops = calculateSurroundingLoops(block)

    def toAst = statementize(translateList(continuation(block, None)))

    /** Calculates the next surrounding loop (or none) for every block. */
    def calculateSurroundingLoops(block: Block, surroundingLoop: Option[LoopBlock] = None, knownSurroundingLoops: Map[Block, Option[LoopBlock]] = Map()): Map[Block, Option[LoopBlock]] = {
      if (knownSurroundingLoops.contains(block))  {
        knownSurroundingLoops
      } else {
        val loops1 = knownSurroundingLoops + (block -> surroundingLoop)
        block match {
          case ConditionalBlock(_, _, thn, els) =>
            val loops2 = calculateSurroundingLoops(els, surroundingLoop, loops1)
            calculateSurroundingLoops(thn, surroundingLoop, loops2)
          case NormalBlock(_, succ) =>
            calculateSurroundingLoops(succ, surroundingLoop, loops1)
          case TerminalBlock(_) =>
            loops1
          case b @ LoopBlock(body, _, _, _, succ) =>
            val loops2 = calculateSurroundingLoops(succ, surroundingLoop, loops1)
            calculateSurroundingLoops(body, Some(b), loops2)
          case ConstrainingBlock(_, body, succ) =>
            /* [Malte] Tried to follow what happens in case of a ConditionalBlock. */
            val loops2 = calculateSurroundingLoops(body, surroundingLoop, loops1)
            calculateSurroundingLoops(succ,surroundingLoop,loops2)
        }
      }
    }

    case class BranchInformation(thn: List[Block], els: List[Block], continuation: List[Block])

    val branchesCache = scala.collection.mutable.HashMap[ConditionalBlock, BranchInformation]()

    /** Finds both branches and the continuation of a conditional block. */
    private def extractBranches(block: ConditionalBlock, previousBlocks: Set[Block] = Set()) = {
      if (branchesCache.contains(block)) {
        branchesCache(block)
      } else {
        val thenContinuation = continuation(block.thn, surroundingLoops(block), previousBlocks + block)
        val elseContinuation = continuation(block.els, surroundingLoops(block), previousBlocks + block)
        val revThnCont = thenContinuation.reverse
        val revElsCont = elseContinuation.reverse
        val revContinuation = revThnCont.zip(revElsCont).takeWhile(a => a._1 eq a._2).unzip._1
        val revThen = revThnCont.drop(revContinuation.size)
        val revElse = revElsCont.drop(revContinuation.size)
        val branchInfo = BranchInformation(revThen.reverse, revElse.reverse, revContinuation.reverse)
        branchesCache.put(block, branchInfo)
        branchInfo
      }
    }

    /** Returns true iff the first loop is equal to or inside the second loop */
    def loopIsInside(first: Option[LoopBlock], second: Option[LoopBlock]): Boolean = {
      if (first == second) {
        true
      } else first match {
        case None => false
        case Some(loop) => loopIsInside(surroundingLoops(loop), second)
      }
    }

    /** Returns the list of one node and all successors in the AST without gotos or loop bodies and overjumping
     * branches of conditionals. */
    private def continuation(block: Block, surroundingLoop: Option[LoopBlock], previousBlocks: Set[Block] = Set()): List[Block] = {
      if (usedBlocks.contains(block) || previousBlocks.contains(block)) {
        sys.error("Backward gotos are not allowed.")
      } else if (surroundingLoops(block) != surroundingLoop) {
        if (loopIsInside(surroundingLoop, surroundingLoops(block))) {
          Nil
        } else {
          sys.error("Jumping into a loop with a goto is not allowed.")
        }
      } else block match {
        case b: ConditionalBlock => b :: extractBranches(b, previousBlocks).continuation
        case b: TerminalBlock => List(b)
        case b @ LoopBlock(body, _, _, _, succ) => b :: continuation(succ, surroundingLoop, previousBlocks + b)
        case b @ ConstrainingBlock(_, _, succ) => b :: continuation(succ, surroundingLoop, previousBlocks + b)
        case b @ NormalBlock(_, succ) => b :: continuation(succ, surroundingLoop, previousBlocks + b)
      }
    }

    private val labels = scala.collection.mutable.HashMap[Block, String]()

    private val usedBlocks = scala.collection.mutable.Set[Block]()

    /** Creates a statement from a list of statements by either using the unique element or a Seqn. */
    def statementize(stmts: List[Stmt]) = stmts match {
      case List(stmt) => stmt
      case _ => Seqn(stmts)()
    }

    /** Returns the label of the given block or creates one if necessary. */
    def label(block: Block) = {
      if (labels contains block) {
        labels(block)
      } else {
        val newLabel = nameGen.createUnique("label")
        labels.put(block, newLabel)
        newLabel
      }
    }

    /** Translates a list of blocks. */
    private def translateList(blocks: List[Block]): List[Stmt] = blocks match {
      case Nil => Nil
      case (b @ NormalBlock(stmt, succ)) :: Nil if surroundingLoops(b) != surroundingLoops(succ) =>
        // Goto at the end of the continuation
        val stmts = translate(b)
        stmts :+ Goto(label(succ))()
      case head :: rest =>
        val stmts = translate(head)
        val restStmts = translateList(rest)
        stmts ++ restStmts
    }

    /** Translates a block into a list of statements. */
    private def translate(block: Block): List[Stmt] = {
      usedBlocks add block
      val translated = block match {
        case TerminalBlock(stmt) => List(stmt)
        case NormalBlock(stmt, _) => List(stmt)
        case b @ ConditionalBlock(stmt, cond, thn, els) =>
          val BranchInformation(thn, els, _) = extractBranches(b)
          val translatedThen = translateList(thn)
          val translatedElse = translateList(els)
          val translatedIf = If(cond, statementize(translatedThen), statementize(translatedElse))()
          List(stmt, translatedIf)
        case b @ LoopBlock(body, cond, invs, locals, succ) =>
          val translatedBody = translateList(continuation(body, Some(b)))
          val translatedLoop = While(cond, invs, locals, statementize(translatedBody))()
          List(translatedLoop)
        case ConstrainingBlock(vars, body, _) =>
          val translatedBody = translateList(continuation(body, None))
          val translatedConstraining = Constraining(vars, statementize(translatedBody))()
          List(translatedConstraining)
      }
      if (labels contains block) {
        Label(labels(block), Seq())() :: translated
      } else {
        translated
      }
    }

  }

}
