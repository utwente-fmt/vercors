package viper.api.backend.silicon

import com.typesafe.scalalogging.LazyLogging
import hre.progress.TaskRegistry
import hre.progress.task.{AbstractTask, Task}
import viper.api.backend.silicon.SiliconLogListener.{
  BranchCondition,
  BranchConditionExp,
  BranchConditionNone,
  BranchConditionTerm,
}
import viper.api.transform.NodeInfo
import viper.silicon.decider.PathConditionStack
import viper.silicon.logger.records.data._
import viper.silicon.logger.records.scoping.{
  CloseScopeRecord,
  OpenScopeRecord,
  ScopingRecord,
}
import viper.silicon.logger.records.structural.BranchingRecord
import viper.silicon.logger.{MemberSymbExLogger, SymbExLogger}
import viper.silicon.state.terms
import viper.silicon.state.terms.Term
import viper.silver.ast._

import java.util.{Timer, TimerTask}
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration.{Duration, DurationInt}
import scala.language.postfixOps

case object SiliconLogListener {
  val NO_PROGRESS_TIMEOUT: Duration = 10 seconds
  val logs: ArrayBuffer[SiliconMemberLogListener] = ArrayBuffer()

  sealed trait BranchCondition
  case class BranchConditionExp(e: Exp) extends BranchCondition
  case class BranchConditionTerm(t: Term) extends BranchCondition
  case class BranchConditionNone(idx: Int, count: Int) extends BranchCondition
}

case class SiliconLogListener(
    reportOnNoProgress: Boolean,
    traceBranchConditions: Boolean,
    branchConditionReportInterval: Option[Int],
) extends SymbExLogger[SiliconMemberLogListener] {
  val superTask: Option[AbstractTask] =
    if (TaskRegistry.enabled)
      Some(TaskRegistry.currentTaskInThread)
    else
      None

  override protected def newEntityLogger(
      member: Member,
      pcs: PathConditionStack,
  ): SiliconMemberLogListener = {
    val log = new SiliconMemberLogListener(this, member, pcs, superTask)
    SiliconLogListener.logs += log
    log
  }
}

class SiliconMemberLogListener(
    log: SiliconLogListener,
    member: Member,
    pcs: PathConditionStack,
    superTask: Option[AbstractTask],
) extends MemberSymbExLogger(log, member, pcs) with LazyLogging {
  var openScopeFrames: List[mutable.Map[Int, DataRecord]] = List(mutable.Map())
  var branchScopeCloseRecords: List[mutable.Set[Int]] = List(mutable.Set())
  var branchConditions: List[BranchCondition] = List()
  var branchUpdates: Int = 0

  var timer = new Timer()
  var currentTimerTask: Option[TimerTask] = None

  def siliconProgress(): Unit = {
    taskProgress()

    if (!log.reportOnNoProgress)
      return

    currentTimerTask.foreach(_.cancel())
    timer.purge()

    currentTimerTask = Some(new TimerTask {
      override def run(): Unit = printDetailedState()
    })

    try {
      timer.schedule(
        currentTimerTask.get,
        SiliconLogListener.NO_PROGRESS_TIMEOUT.toMillis,
      )
    } catch {
      case e: IllegalStateException =>
        println("what")
        e.printStackTrace()
    }
  }

  def done(): Unit = {
    timer.cancel()
    if (superTask.nonEmpty)
      currentTaskStack = updateTaskStack(
        currentTaskStack,
        superTask.get,
        Nil,
        Nil,
      )
  }

  def where(node: Node): Option[String] =
    Util.getOrigin(node).map(_.shortPosition)

  def printRecords(
      records: mutable.Map[Int, DataRecord],
      excludedBy: Map[Int, Int],
  ): Unit = {
    for (record <- records.values.toSeq.sortBy(_.id)) {
      val at =
        record match {
          case member: MemberRecord => where(member.value)
          case exec: ExecuteRecord => where(exec.value)
          case produce: ProduceRecord => where(produce.value)
          case consume: ConsumeRecord => where(consume.value)
          case _ => None
        }

      if (at.nonEmpty) { logger.info(s"    At ${at.get}:") }

      if (excludedBy.contains(record.id)) {
        logger
          .info(s"      [finished in branch ${excludedBy(record.id)}]: $record")
      } else { logger.info(s"      $record") }
    }
  }

  def printDetailedState(): Unit = {
    val exclude = branchScopeCloseRecords.flatMap(_.toSeq).toSet

    val excludedBy =
      branchScopeCloseRecords.zipWithIndex.flatMap { case (excluded, idx) =>
        excluded.map(_ -> idx)
      }.toMap

    logger.warn(
      "Silicon has been working on the same proof goal for over 10 seconds."
    )
    logger.info("Current state of silicon worker:")
    printRecords(openScopeFrames.last, excludedBy)

    for (
      ((records, idx), condition) <-
        openScopeFrames.init.zipWithIndex.zip(branchConditions).reverse
    ) {
      condition match {
        case BranchConditionExp(cond) => logger.info(s"  [$idx] $cond")
        case BranchConditionTerm(cond) => logger.info(s"  [$idx] $cond")
        case BranchConditionNone(at, count) =>
          logger.info(s"  [$idx] <indeterminate branch ${at + 1} of $count>")
      }

      printRecords(records, excludedBy)
    }
  }

  var currentTaskStack: Seq[Task] = Nil

  def updateTaskStack(
      taskStack: Seq[Task],
      superTask: AbstractTask,
      records: Seq[DataRecord],
      branches: Seq[BranchCondition],
  ): Seq[Task] =
    (taskStack, records, branches) match {
      case (Nil, Nil, Nil) => Nil
      case (tasks, Nil, Nil) =>
        tasks.reverse.foreach(_.end())
        Nil

      case (Nil, Nil, branch +: branches) =>
        val task = BranchRecordTask(superTask, branch)
        task.start()
        task +: updateTaskStack(Nil, task, Nil, branches)
      case (task +: tasks, Nil, branch +: branches) =>
        if (task == BranchRecordTask(superTask, branch)) {
          task +: updateTaskStack(tasks, task, Nil, branches)
        } else {
          tasks.reverse.foreach(_.end())
          task.end()
          updateTaskStack(Nil, superTask, Nil, branch +: branches)
        }

      case (Nil, record +: records, _) =>
        val task = DataRecordTask(superTask, record)
        task.start()
        task +: updateTaskStack(Nil, task, records, branches)
      case (task +: tasks, record +: records, _) =>
        if (
          task.superTask == superTask &&
          task == DataRecordTask(superTask, record)
        ) { task +: updateTaskStack(tasks, task, records, branches) }
        else {
          tasks.reverse.foreach(_.end())
          task.end()
          updateTaskStack(Nil, superTask, record +: records, branches)
        }
    }

  def taskProgress(): Unit = {
    if (this.superTask.isEmpty)
      return
    val superTask = this.superTask.get

    val BANNED_COMMENTS = Set("Retry")

    val records = openScopeFrames.reverse.flatMap(_.values)
      .filter(r => !branchScopeCloseRecords.exists(_.contains(r.id))).collect {
        case r: MemberRecord => r
        case r: ExecuteRecord => r
        case r: ConsumeRecord => r
        case r: ProduceRecord => r
        case r: CommentRecord if !BANNED_COMMENTS.contains(r.comment) => r
      }

    val conditions = branchConditions.collect { case BranchConditionExp(e) =>
      BranchConditionExp(e)
    }

    currentTaskStack = updateTaskStack(
      currentTaskStack,
      superTask,
      records,
      conditions,
    )
  }

  def updateBranch(indicator: String): Unit = {
    if (log.traceBranchConditions) {
      val textCond =
        branchConditions.head match {
          case BranchConditionExp(e) => e.toString()
          case BranchConditionTerm(e) => e.toString
          case BranchConditionNone(at, count) => s"alternative $at/$count"
        }
      val indent = "  ".repeat(branchConditions.size)
      logger.info(s"${member.name}: ${indent}${indicator} ${textCond}")
    }

    branchUpdates += 1
    val modulus = log.branchConditionReportInterval
    if (modulus.nonEmpty && branchUpdates % modulus.get == 0) {
      logger.warn(
        s"Silicon has explored ${branchUpdates} branch traces for entity ${member.name}."
      )
      logger.info("Current branch information:")
      // PB: heuristic: the oldest branch is earliest in the text input, so present that first.
      branchConditions.reverse.foreach {
        case BranchConditionExp(e) =>
          where(e) match {
            case None => logger.info(s" - $e")
            case Some(pos) => logger.info(s" - At $pos: $e")
          }
        case BranchConditionTerm(t) => logger.info(s" - $t")
        case BranchConditionNone(at, count) =>
          logger.info(s" - alternative ${at + 1} of $count")
      }
    }
  }

  override def appendDataRecord(r: DataRecord): Unit = {
    openScopeFrames.head(r.id) = r
    siliconProgress()
  }

  override def appendScopingRecord(
      r: ScopingRecord,
      ignoreBranchingStack: Boolean,
  ): Unit = {
    r match {
      case r: CloseScopeRecord =>
        if (openScopeFrames.head.contains(r.refId)) {
          openScopeFrames.head.remove(r.refId)
        } else { branchScopeCloseRecords.head += r.refId }
      case _: OpenScopeRecord => // This is just done from datarecord; safe to ignore.
    }

    r match {
      case r: CloseScopeRecord if r.refId == main.id => done()
      case _ => siliconProgress()
    }
  }

  override def appendBranchingRecord(r: BranchingRecord): Unit = {
    openScopeFrames +:= mutable.Map()
    branchScopeCloseRecords +:= mutable.Set()

    if (r.getBranchInfos.size == 2) {
      if (r.conditionExp.nonEmpty) {
        branchConditions +:= BranchConditionExp(r.conditionExp.get)
      } else if (r.condition.nonEmpty) {
        branchConditions +:= BranchConditionTerm(r.condition.get)
      } else { branchConditions +:= BranchConditionNone(0, r.getBranches.size) }
    } else { branchConditions +:= BranchConditionNone(0, r.getBranches.size) }

    updateBranch("->")
    siliconProgress()
  }

  def invert(term: Term): Term =
    term match {
      case terms.Not(e) => e
      case other => terms.Not(other)
    }

  def invert(e: Exp): Exp =
    e match {
      case Not(e) => e
      case other => Not(other)(pos = e.pos, info = e.info)
    }

  def advanceBranch(): Unit =
    branchConditions =
      (branchConditions.head match {
        case BranchConditionExp(e) => BranchConditionExp(invert(e))
        case BranchConditionTerm(t) => BranchConditionTerm(invert(t))
        case BranchConditionNone(at, count) =>
          BranchConditionNone(at + 1, count)
      }) +: branchConditions.tail

  override def doSwitchToNextBranch(uidBranchPoint: Int): Unit = {
    openScopeFrames.head.clear()
    branchScopeCloseRecords.head.clear()

    advanceBranch()

    updateBranch("->")

    siliconProgress()
  }

  override def markBranchReachable(uidBranchPoint: Int): Unit = {
    siliconProgress()
  }

  override def doEndBranchPoint(uidBranchPoint: Int): Unit = {
    advanceBranch()
    updateBranch("<-")
    openScopeFrames = openScopeFrames.tail

    for (closeRecord <- branchScopeCloseRecords.head) {
      for (frame <- openScopeFrames) { frame.remove(closeRecord) }
    }

    branchScopeCloseRecords = branchScopeCloseRecords.tail
    branchConditions = branchConditions.tail

    siliconProgress()
  }
}
