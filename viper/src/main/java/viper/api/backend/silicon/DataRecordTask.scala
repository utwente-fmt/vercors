package viper.api.backend.silicon

import hre.progress.task.{AbstractTask, Task}
import viper.silicon.logger.records.data.{CommentRecord, ConsumeRecord, DataRecord, ExecuteRecord, FunctionRecord, MethodRecord, PredicateRecord, ProduceRecord}

case class DataRecordTask(superTask: AbstractTask, record: DataRecord) extends Task {
  override def progressText: String = record match {
    case r: FunctionRecord => Util.getOrigin(r.value).map(_.messageInContext("Verifying")).getOrElse(r.value.name)
    case r: PredicateRecord => Util.getOrigin(r.value).map(_.messageInContext("Verifying")).getOrElse(r.value.name)
    case r: MethodRecord => Util.getOrigin(r.value).map(_.messageInContext("Verifying")).getOrElse(r.value.name)
    case r: ExecuteRecord => Util.getOrigin(r.value).map(_.messageInContext("Executing")).getOrElse(r.value.toString())
    case r: ConsumeRecord => Util.getOrigin(r.value).map(_.messageInContext("Exhaling")).getOrElse(r.value.toString())
    case r: ProduceRecord => Util.getOrigin(r.value).map(_.messageInContext("Inhaling")).getOrElse(r.value.toString())
    case r: CommentRecord => s"/*${r.comment}*/"
  }

  override def profilingBreadcrumb: String = record match {
    case r: FunctionRecord => s"${r.value.name}"
    case r: PredicateRecord => s"${r.value.name}"
    case r: MethodRecord => s"${r.value.name}"
    case r: ExecuteRecord => s"${r.value.toString().replaceAll("[\n ]+", " ")}"
    case r: ConsumeRecord => s"Exhale ${r.value.toString().replaceAll("[\n ]+", " ")}"
    case r: ProduceRecord => s"Inhale ${r.value.toString().replaceAll("[\n ]+", " ")}"
    case r: CommentRecord => s"/*${r.comment}*/"
  }

  override def hashCode(): Int = record.id
  override def equals(other: Any): Boolean = other match {
    case DataRecordTask(_, other) => other.id == record.id
    case _ => false
  }
}
