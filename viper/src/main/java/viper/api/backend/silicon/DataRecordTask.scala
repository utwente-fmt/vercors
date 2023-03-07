package viper.api.backend.silicon

import hre.progress.task.Task
import viper.silicon.logger.records.data.{ConsumeRecord, DataRecord, ExecuteRecord, FunctionRecord, MethodRecord, PredicateRecord, ProduceRecord}

case class DataRecordTask(superTask: Task, record: DataRecord) extends Task {
  override def progressText: String = record match {
    case r: FunctionRecord => r.value.name
    case r: PredicateRecord => r.value.name
    case r: MethodRecord => r.value.name
    case r: ExecuteRecord => r.value.toString()
    case r: ConsumeRecord => r.value.toString()
    case r: ProduceRecord => r.value.toString()
  }

  override def profilingBreadcrumb: String = progressText
}
