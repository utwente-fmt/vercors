package viper.api.backend.silicon

import hre.progress.ProgressRender
import hre.progress.task.{AbstractTask, Task}
import viper.silicon.logger.records.data.{CommentRecord, ConsumeRecord, DataRecord, DeciderAssertRecord, ExecuteRecord, FunctionRecord, MethodRecord, PredicateRecord, ProduceRecord, ProverAssertRecord}

case class DataRecordTask(superTask: AbstractTask, record: DataRecord) extends Task {
  def renderMaybeShort(short: Boolean): ProgressRender = record match {
    case r: FunctionRecord => Util.renderOrigin(r.value, "Verifying", short = true)
    case r: PredicateRecord => Util.renderOrigin(r.value, "Verifying", short = true)
    case r: MethodRecord => Util.renderOrigin(r.value, "Verifying", short = true)
    case r: ExecuteRecord => Util.renderOrigin(r.value, "Executing", short)
    case r: ConsumeRecord => Util.renderOrigin(r.value, "Exhaling", short)
    case r: ProduceRecord => Util.renderOrigin(r.value, "Inhaling", short)
    case r: CommentRecord => ProgressRender(s"/*${r.comment}*/")
    case r: DeciderAssertRecord => ProgressRender(s"Asserting ${r.term.toString.replaceAll("[\n ]+", " ")}")
    case r: ProverAssertRecord =>  ProgressRender(s"Asserting ${r.term.toString.replaceAll("[\n ]+", " ")}")
  }

  override def renderHere: ProgressRender = renderMaybeShort(short = false)
  override def renderHereShort: ProgressRender = renderMaybeShort(short = true)

  override def profilingBreadcrumb: String = record match {
    case r: FunctionRecord => s"Function ${r.value.name}"
    case r: PredicateRecord => s"Predicate ${r.value.name}"
    case r: MethodRecord => s"Method ${r.value.name}"
    case r: ExecuteRecord => s"${CachedExpRender(r.value).replaceAll("[\n ]+", " ")}"
    case r: ConsumeRecord => s"Exhale ${CachedExpRender(r.value).replaceAll("[\n ]+", " ")}"
    case r: ProduceRecord => s"Inhale ${CachedExpRender(r.value).replaceAll("[\n ]+", " ")}"
    case r: CommentRecord => s"/*${r.comment}*/"
    case r: DeciderAssertRecord => s"Assert ${r.term.toString.replaceAll("[\n ]+", " ")}"
    case r: ProverAssertRecord => s"Assert ${r.term.toString.replaceAll("[\n ]+", " ")}"
  }

  override def hashCode(): Int = record.id
  override def equals(other: Any): Boolean = other match {
    case DataRecordTask(_, other) => other.id == record.id
    case _ => false
  }
}
