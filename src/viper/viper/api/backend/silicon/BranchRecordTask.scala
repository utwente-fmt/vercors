package viper.api.backend.silicon

import hre.progress.ProgressRender
import hre.progress.task.{AbstractTask, Task}
import viper.api.backend.silicon.SiliconLogListener.{
  BranchCondition,
  BranchConditionExp,
  BranchConditionNone,
  BranchConditionTerm,
}

case class BranchRecordTask(superTask: AbstractTask, cond: BranchCondition)
    extends Task {
  override def profilingBreadcrumb: String =
    cond match {
      case BranchConditionExp(e) => CachedExpRender(e)
      case BranchConditionTerm(e) => e.toString
      case BranchConditionNone(at, count) => s"alternative $at/$count"
    }

  override def renderHere: ProgressRender = ProgressRender(profilingBreadcrumb)
}
