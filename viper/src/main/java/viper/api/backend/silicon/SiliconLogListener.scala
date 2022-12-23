package viper.api.backend.silicon

import com.typesafe.scalalogging.LazyLogging
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
import scala.concurrent.duration.{Duration, DurationInt}
import scala.language.postfixOps

case object SiliconLogListener extends SymbExLogger[SiliconLogListener] {
  val NO_PROGRESS_TIMEOUT: Duration = 10 seconds

  override protected def newEntityLogger(member: Member, pcs: PathConditionStack): SiliconLogListener =
    new SiliconLogListener(member, pcs)
}

class SiliconLogListener(member: Member, pcs: PathConditionStack) extends MemberSymbExLogger(SiliconLogListener, member, pcs) with LazyLogging {
  var openScopeFrames: List[mutable.Map[Int, DataRecord]] = List(mutable.Map())
  var branchScopeCloseRecords: List[mutable.Set[Int]] = List(mutable.Set())
  var branchConditions: List[Option[Either[Term, Exp]]] = List()

  var timer = new Timer()
  var currentTimerTask: Option[TimerTask] = None

  var doneTrace: Array[StackTraceElement] = _

  def progress(): Unit = {
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
        logger.warn(s"    At ${at.get}:")
      }

      if(excludedBy.contains(record.id)) {
        logger.warn(s"      [finished in branch ${excludedBy(record.id)}]: $record")
      } else {
        logger.warn(s"      $record")
      }
    }
  }

  def printDetailedState(): Unit = {
    val exclude = branchScopeCloseRecords.flatMap(_.toSeq).toSet

    val excludedBy = branchScopeCloseRecords.zipWithIndex.flatMap {
      case (excluded, idx) => excluded.map(_ -> idx)
    }.toMap

    logger.warn(s"Current state of silicon worker:")
    printRecords(openScopeFrames.last, excludedBy)

    for(((records, idx), condition) <- openScopeFrames.init.zipWithIndex.zip(branchConditions).reverse) {
      condition match {
        case Some(Right(cond)) =>
          logger.warn(s"  [$idx] $cond")
        case Some(Left(cond)) =>
          logger.warn(s"  [$idx] $cond")
        case None =>
          logger.warn(s"  [$idx] <indeterminate branch>")
      }

      printRecords(records, excludedBy)
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
        branchConditions +:= Some(Right(r.conditionExp.get))
      } else if(r.condition.nonEmpty) {
        branchConditions +:= Some(Left(r.condition.get))
      } else {
        branchConditions +:= None
      }
    } else {
      branchConditions +:= None
    }
  }

  def invert(termOrExp: Either[Term, Exp]): Either[Term, Exp] = termOrExp match {
    case Right(exp) => Right(exp match {
      case Not(e) => e
      case other => Not(other)()
    })
    case Left(term) => Left(term match {
      case terms.Not(e) => e
      case other => terms.Not(other)
    })
  }

  override def doSwitchToNextBranch(uidBranchPoint: Int): Unit = {
    progress()
    openScopeFrames.head.clear()
    branchScopeCloseRecords.head.clear()

    if(branchConditions.last.nonEmpty) {
      branchConditions = branchConditions.init :+ Some(invert(branchConditions.last.get))
    }
  }

  override def markBranchReachable(uidBranchPoint: Int): Unit = {
    progress()
  }

  override def doEndBranchPoint(uidBranchPoint: Int): Unit = {
    progress()
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
