package viper.api.backend.silicon

import com.typesafe.scalalogging.LazyLogging
import hre.progress.Progress
import vct.col.ast.Neq
import viper.api.transform.NodeInfo
import viper.silicon.decider.PathConditionStack
import viper.silicon.logger.records.data.{ConsumeRecord, DataRecord, ExecuteRecord, MemberRecord, ProduceRecord}
import viper.silicon.logger.records.scoping.{CloseScopeRecord, OpenScopeRecord, ScopingRecord}
import viper.silicon.logger.records.structural.BranchingRecord
import viper.silicon.logger.{LogConfig, MemberSymbExLogger, SymbExLogger}
import viper.silicon.state.{State, terms}
import viper.silicon.state.terms.Term
import viper.silver.ast.{Exp, Infoed, Member, Node, Not, Positioned}

import java.util.{Timer, TimerTask}
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration.{Duration, DurationInt}
import scala.language.postfixOps

case object SiliconLogListener {
  val NO_PROGRESS_TIMEOUT: Duration = 10 seconds
  val logs: ArrayBuffer[SiliconMemberLogListener] = ArrayBuffer()
}

case class SiliconLogListener(reportOnNoProgress: Boolean,
                              traceBranchConditions: Boolean,
                              branchConditionReportInterval: Option[Int]) extends SymbExLogger[SiliconMemberLogListener] {
  override protected def newEntityLogger(member: Member, pcs: PathConditionStack): SiliconMemberLogListener = {
    val log = new SiliconMemberLogListener(this, member, pcs)
    SiliconLogListener.logs += log
    log
  }
}

class SiliconMemberLogListener(log: SiliconLogListener, member: Member, pcs: PathConditionStack) extends MemberSymbExLogger(log, member, pcs) with LazyLogging {
  sealed trait BranchCondition
  case class BranchConditionExp(e: Exp) extends BranchCondition
  case class BranchConditionTerm(t: Term) extends BranchCondition
  case class BranchConditionNone(idx: Int, count: Int) extends BranchCondition

  var openScopeFrames: List[mutable.Map[Int, DataRecord]] = List(mutable.Map())
  var branchScopeCloseRecords: List[mutable.Set[Int]] = List(mutable.Set())
  var branchConditions: List[BranchCondition] = List()
  var branchUpdates: Int = 0

  var timer = new Timer()
  var currentTimerTask: Option[TimerTask] = None

  var doneTrace: Array[StackTraceElement] = _

  def progress(): Unit = {
    if(!log.reportOnNoProgress) return

    currentTimerTask.foreach(_.cancel())
    timer.purge()

    currentTimerTask = Some(new TimerTask {
      override def run(): Unit = printDetailedState()
    })

    try {
      timer.schedule(currentTimerTask.get, SiliconLogListener.NO_PROGRESS_TIMEOUT.toMillis)
    } catch {
      case _: IllegalStateException =>
        println("what")
    }
  }

  def done(): Unit = {
    timer.cancel()
    doneTrace = Thread.currentThread().getStackTrace
  }

  def where(node: Node): Option[String] = node match {
    case node: Infoed => node.info.getUniqueInfo[NodeInfo[vct.col.ast.Node[_]]].map(_.node.o.shortPosition)
    case _ => None
  }

  def which(node: Node): Option[vct.col.ast.Node[_]] = node match {
    case node: Infoed => node.info.getUniqueInfo[NodeInfo[vct.col.ast.Node[_]]].map(_.node)
    case _ => None
  }

  def printRecords(records: mutable.Map[Int, DataRecord], excludedBy: Map[Int, Int]): Unit = {
    for(record <- records.values.toSeq.sortBy(_.id)) {
      val at = record match {
        case member: MemberRecord => where(member.value)
        case exec: ExecuteRecord => where(exec.value)
        case produce: ProduceRecord => where(produce.value)
        case consume: ConsumeRecord => where(consume.value)
        case _ => None
      }

      if(at.nonEmpty) {
        logger.info(s"    At ${at.get}:")
      }

      if(excludedBy.contains(record.id)) {
        logger.info(s"      [finished in branch ${excludedBy(record.id)}]: $record")
      } else {
        logger.info(s"      $record")
      }
    }
  }

  def printDetailedState(): Unit = {
    val exclude = branchScopeCloseRecords.flatMap(_.toSeq).toSet

    val excludedBy = branchScopeCloseRecords.zipWithIndex.flatMap {
      case (excluded, idx) => excluded.map(_ -> idx)
    }.toMap

    logger.warn("Silicon has been working on the same proof goal for over 10 seconds.")
    logger.info("Current state of silicon worker:")
    printRecords(openScopeFrames.last, excludedBy)

    for(((records, idx), condition) <- openScopeFrames.init.zipWithIndex.zip(branchConditions).reverse) {
      condition match {
        case BranchConditionExp(cond) =>
          logger.info(s"  [$idx] $cond")
        case BranchConditionTerm(cond) =>
          logger.info(s"  [$idx] $cond")
        case BranchConditionNone(at, count) =>
          logger.info(s"  [$idx] <indeterminate branch ${at+1} of $count>")
      }

      printRecords(records, excludedBy)
    }
  }

  def updateBranch(indicator: String): Unit = {
    if(log.traceBranchConditions) {
      val textCond = branchConditions.head match {
        case BranchConditionExp(e) => e.toString()
        case BranchConditionTerm(e) => e.toString
        case BranchConditionNone(at, count) => s"alternative $at/$count"
      }
      val indent = "  ".repeat(branchConditions.size)
      logger.info(s"${member.name}: ${indent}${indicator} ${textCond}")
    }

    branchUpdates += 1
    val modulus = log.branchConditionReportInterval
    if(modulus.nonEmpty && branchUpdates % modulus.get == 0) {
      logger.warn(s"Silicon has explored ${branchUpdates} branch traces for entity ${member.name}.")
      logger.info("Current branch information:")
      // PB: heuristic: the oldest branch is earliest in the text input, so present that first.
      branchConditions.reverse.foreach {
        case BranchConditionExp(e) => where(e) match {
          case None =>
            logger.info(s" - $e")
          case Some(pos) =>
            logger.info(s" - At $pos: $e")
        }
        case BranchConditionTerm(t) =>
          logger.info(s" - $t")
        case BranchConditionNone(at, count) =>
          logger.info(s" - alternative ${at+1} of $count")
      }
    }
  }

  override def appendDataRecord(r: DataRecord): Unit = {
    progress()
    openScopeFrames.head(r.id) = r
  }

  override def appendScopingRecord(r: ScopingRecord, ignoreBranchingStack: Boolean): Unit = {
    progress()
    r match {
      case r: CloseScopeRecord =>
        if(r.refId == main.id)
          done()

        if(openScopeFrames.head.contains(r.refId)) {
          openScopeFrames.head.remove(r.refId)
        } else {
          branchScopeCloseRecords.head += r.refId
        }
      case _: OpenScopeRecord => // This is just done from datarecord; safe to ignore.
    }
  }

  override def appendBranchingRecord(r: BranchingRecord): Unit = {
    progress()
    openScopeFrames +:= mutable.Map()
    branchScopeCloseRecords +:= mutable.Set()

    if(r.getBranchInfos.size == 2) {
      if(r.conditionExp.nonEmpty) {
        branchConditions +:= BranchConditionExp(r.conditionExp.get)
      } else if(r.condition.nonEmpty) {
        branchConditions +:= BranchConditionTerm(r.condition.get)
      } else {
        branchConditions +:= BranchConditionNone(0, r.getBranches.size)
      }
    } else {
      branchConditions +:= BranchConditionNone(0, r.getBranches.size)
    }

    updateBranch("->")
  }

  def invert(term: Term): Term = term match {
    case terms.Not(e) => e
    case other => terms.Not(other)
  }

  def invert(e: Exp): Exp = e match {
    case Not(e) => e
    case other => Not(other)(pos = e.pos, info = e.info)
  }

  def advanceBranch(): Unit =
    branchConditions = (branchConditions.head match {
      case BranchConditionExp(e) => BranchConditionExp(invert(e))
      case BranchConditionTerm(t) => BranchConditionTerm(invert(t))
      case BranchConditionNone(at, count) => BranchConditionNone(at + 1, count)
    }) +: branchConditions.tail

  override def doSwitchToNextBranch(uidBranchPoint: Int): Unit = {
    progress()
    openScopeFrames.head.clear()
    branchScopeCloseRecords.head.clear()

    advanceBranch()

    updateBranch("->")
  }

  override def markBranchReachable(uidBranchPoint: Int): Unit = {
    progress()
  }

  override def doEndBranchPoint(uidBranchPoint: Int): Unit = {
    progress()
    advanceBranch()
    updateBranch("<-")
    openScopeFrames = openScopeFrames.tail

    for(closeRecord <- branchScopeCloseRecords.head) {
      for(frame <- openScopeFrames) {
        frame.remove(closeRecord)
      }
    }

    branchScopeCloseRecords = branchScopeCloseRecords.tail
    branchConditions = branchConditions.tail
  }
}
