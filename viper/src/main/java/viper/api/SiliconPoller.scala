package viper.api

import com.typesafe.scalalogging.{LazyLogging, Logger}
import vct.col.ast.{Expr, Statement}
import viper.silicon.logger.records.SymbolicRecord
import viper.silicon.logger.records.data.{ConsumeRecord, ExecuteRecord}
import viper.silicon.logger.records.scoping.{CloseScopeRecord, OpenScopeRecord}
import viper.silicon.logger.records.structural.BranchingRecord
import viper.silicon.logger.{NoopSymbLog, SymbExLogger}
import viper.silver.ast.{Exp, Stmt}

import java.util.{Timer, TimerTask}
import scala.collection.mutable

case object SiliconPoller {
  val logger: Logger = Logger[SiliconPoller]

  sealed trait SiliconState {
    def log(): Unit
  }

  case class Executing(stat: Stmt) extends SiliconState {
    def col: Statement[_] = stat.info.getUniqueInfo[NodeInfo[Statement[_]]].get.node
    override def log(): Unit =
      logger.warn(col.o.messageInContext("Silicon is executing this statement"))
  }

  case class Exhaling(exp: Exp) extends SiliconState {
    def col: Expr[_] = exp.info.getUniqueInfo[NodeInfo[Expr[_]]].get.node
    override def log(): Unit =
      logger.warn(col.o.messageInContext("Silicon is exhaling this resource"))
  }
}

case class SiliconPoller(interval: Long) extends LazyLogging {
  import SiliconPoller._

  val timer = new Timer()

  def pollInScope[T](f: => T): T =
    try {
      timer.schedule(new TimerTask {
        override def run(): Unit = poll()
      }, 0, interval)

      f
    } finally {
      timer.cancel()
    }

  def allNonClosed(log: Seq[SymbolicRecord]): Seq[SymbolicRecord] =
    log.foldRight((Seq[SymbolicRecord](), Set.empty[Int])) {
      case (record, (result, closed)) => record match {
        case close: CloseScopeRecord =>
          (result, closed ++ Set(close.refId))
        case _: OpenScopeRecord | _: BranchingRecord =>
          (result, closed)
        case other if !closed.contains(other.id) =>
          (record +: result, closed)
        case _ => (result, closed)
      }
    }._1

  def findNonClosed[T <: SymbolicRecord](log: Seq[SymbolicRecord])(f: PartialFunction[SymbolicRecord, T]): Option[T] = {
    val closed: mutable.Set[Int] = mutable.Set()

    for(entry <- log.reverseIterator) {
      entry match {
        case close: CloseScopeRecord =>
          closed += close.refId
        case other =>
          f.lift(other) match {
            case None => // continue searching
            case Some(record) =>
              // matched, so check it's not closed
              if(closed.contains(record.id)) {
                return None
              } else {
                return Some(record)
              }
          }
      }
    }

    None
  }

  def executing(log: Seq[SymbolicRecord]): Option[Executing] =
    findNonClosed(log) {
      case r: ExecuteRecord => r
    }.map(r => Executing(r.value))

  def exhaling(log: Seq[SymbolicRecord]): Option[Exhaling] =
    findNonClosed(log) {
      case r: ConsumeRecord => r
    }.map(r => Exhaling(r.value))

  def poll(): Unit = {
//    val symbLog = SymbExLogger.currentLog()
//    if(symbLog == NoopSymbLog) return

//    val log = symbLog.log ++ symbLog.branchingStack.flatMap(_.getCurrentBranch.records)
//
//    logger.warn("")
//    logger.warn("=" * 80)
//
//    for(branch <- symbLog.branchingStack) {
//      if(branch.getBranches.size == 2) {
//        if(branch.getCurrentBranch == branch.getBranchInfos.head) {
//          logger.warn(s"- On the true branch of ${branch.condition}")
//        } else {
//          logger.warn(s"- On the false branch of ${branch.condition}")
//        }
//      } else {
//        logger.warn(s"- On branch ${branch.condition} / ${branch.getCurrentBranch}")
//      }
//    }

//    allNonClosed(log).foreach { entry =>
//      logger.warn(s"${entry.getClass.getSimpleName}#${entry.##} $entry")
//    }

//    log.main match {
//      case record: FunctionRecord => logger.warn(s"Silicon is verifying function ${record.value.name}")
//      case record: MethodRecord => logger.warn(s"Silicon is verifying method ${record.value.name}")
//      case record: PredicateRecord => logger.warn(s"Silicon is verifying predicate ${record.value.name}")
//    }
//
//    executing(log) orElse
//      exhaling(log)
//    match {
//      case Some(state) => state.log()
//      case None =>
//        logger.warn("However, we're not sure what is being verified.")
//        logger.warn(log.log.takeRight(20).toString())
//    }
  }
}