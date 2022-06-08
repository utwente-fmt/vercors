package viper.api

import com.typesafe.scalalogging.LazyLogging
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

  def currentOpenScopes: Seq[(Option[Exp], Seq[DataRecord])] = {
    val exclude = branchScopeCloseRecords.flatMap(_.toSeq).toSet
    (None +: branchConditions).zip(
      openScopeFrames.map(_.values.filterNot(r => exclude.contains(r.id)).toSeq.sortBy(_.id)))
  }

  override def appendDataRecord(symbLog: SymbLog, r: DataRecord): Unit = {
    openScopeFrames.head(r.id) = r
    logger.warn(currentOpenScopes.toString())
  }

  override def appendScopingRecord(symbLog: SymbLog, r: ScopingRecord, ignoreBranchingStack: Boolean): Unit =
    r match {
      case r: CloseScopeRecord =>
        if(openScopeFrames.head.contains(r.refId)) {
          openScopeFrames.head.remove(r.refId)
        } else {
          branchScopeCloseRecords.head += r.refId
        }
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
    openScopeFrames = openScopeFrames.init
    branchScopeCloseRecords = branchScopeCloseRecords.init
    branchConditions = branchConditions.init
  }
}
