package viper.api

import com.typesafe.scalalogging.LazyLogging
import vct.col.ast.Expr
import viper.silicon.logger.records.data.DataRecord
import viper.silicon.logger.records.scoping.{CloseScopeRecord, OpenScopeRecord, ScopingRecord}
import viper.silicon.logger.records.structural.BranchingRecord
import viper.silicon.logger.{SymbLog, SymbLogListener}
import viper.silver.ast.Exp

import scala.collection.mutable

case class SiliconLogListener() extends SymbLogListener with LazyLogging {
  var openScopeFrames: List[mutable.Map[Int, DataRecord]] = List(mutable.Map())
  var branchScopeCloseRecords: List[mutable.Set[Int]] = List(mutable.Set())
  var branchConditions: List[Option[Exp]] = List()

  def printRecords(records: mutable.Map[Int, DataRecord], excludedBy: Map[Int, Int]): Unit = {
    for(record <- records.values.toSeq.sortBy(_.id)) {
      if(excludedBy.contains(record.id)) {
        logger.warn(s"    [finished in branch ${excludedBy(record.id)}]: $record")
      } else {
        logger.warn(s"    $record")
      }
    }
  }

  def currentOpenScopes(symbLog: SymbLog): Seq[(Option[Exp], Seq[DataRecord])] = {
    val exclude = branchScopeCloseRecords.flatMap(_.toSeq).toSet

    val excludedBy = branchScopeCloseRecords.zipWithIndex.flatMap {
      case (excluded, idx) => excluded.map(_ -> idx)
    }.toMap

    logger.warn(s"State of ${symbLog.v.name}:")
    printRecords(openScopeFrames.last, excludedBy)

    for(((records, idx), condition) <- openScopeFrames.init.zipWithIndex.zip(branchConditions).reverse) {
      condition match {
        case Some(cond) =>
          logger.warn(s"  [$idx] ? $cond")
        case None =>
          logger.warn(s"  [$idx] ?")
      }

      printRecords(records, excludedBy)
    }

    (branchConditions :+ None)
      .zip(openScopeFrames.map(_.values.filterNot(r => exclude.contains(r.id))
                          .toSeq.sortBy(_.id)))
      .reverse
  }

  override def appendDataRecord(symbLog: SymbLog, r: DataRecord): Unit = {
    openScopeFrames.head(r.id) = r
    getClass.synchronized {
      currentOpenScopes(symbLog)
    }
  }

  override def appendScopingRecord(symbLog: SymbLog, r: ScopingRecord, ignoreBranchingStack: Boolean): Unit =
    r match {
      case r: CloseScopeRecord =>
        if(openScopeFrames.head.contains(r.refId)) {
          openScopeFrames.head.remove(r.refId)
        } else {
          branchScopeCloseRecords.head += r.refId
        }

        currentOpenScopes(symbLog)
      case _: OpenScopeRecord => // This is just done from datarecord; safe to ignore.
    }

  override def appendBranchingRecord(symbLog: SymbLog, r: BranchingRecord): Unit = {
    openScopeFrames +:= mutable.Map()
    branchScopeCloseRecords +:= mutable.Set()
    branchConditions +:= r.conditionExp
  }

  override def switchToNextBranch(symbLog: SymbLog, uidBranchPoint: Int): Unit = {
    openScopeFrames.head.clear()
    branchScopeCloseRecords.head.clear()
  }

  override def markBranchReachable(symbLog: SymbLog, uidBranchPoint: Int): Unit = {
    // ignore for now
  }

  override def endBranchPoint(symbLog: SymbLog, uidBranchPoint: Int): Unit = {
    openScopeFrames = openScopeFrames.tail

    for(closeRecord <- branchScopeCloseRecords.head) {
      for(frame <- openScopeFrames) {
        frame.remove(closeRecord) match {
          case Some(record) =>
            logger.warn(s"From ${symbLog.v.name}: record deleted after end branch: ${record}")
          case None => // ok
        }
      }
    }

    branchScopeCloseRecords = branchScopeCloseRecords.tail
    branchConditions = branchConditions.tail
  }
}
