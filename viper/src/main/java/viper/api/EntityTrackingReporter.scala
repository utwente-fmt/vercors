package viper.api

import hre.progress.Progress
import viper.silver.ast.{Member, Program}
import viper.silver.reporter.{AstConstructionResultMessage, BackendSubProcessReport, CachedEntityMessage, EntityFailureMessage, EntitySuccessMessage, ExceptionReport, ExecutionTraceReport, ExternalDependenciesReport, InvalidArgumentsReport, Message, OverallFailureMessage, OverallSuccessMessage, ProgramDefinitionsReport, ProgramOutlineReport, Reporter, SimpleMessage, StatisticsReport, VerificationResultMessage, WarningsDuringParsing, WarningsDuringTypechecking}

import scala.collection.mutable

case class EntityTrackingReporter() extends Reporter {
  override val name: String = "entity_tracking_reporter"

  private val todo: mutable.Set[Member] = mutable.Set()

  def todoMessages: String = {
    val entities = todo.map(_.name).toSeq.sorted
    "Entities left: " + (
      if(entities.size > 4) entities.take(4).mkString("", ", ", ", ...")
      else entities.mkString(", ")
    )
  }

  def withEntities[T](program: Program)(f: => T): T = {
    todo.clear()
    todo ++= program.functions
    todo ++= program.predicates
    todo ++= program.methods

    Progress.dynamicMessages(todo.size, todoMessages)(f)
  }

  private def update(): Unit = {
    Progress.nextPhase(todoMessages)
  }

  override def report(msg: Message): Unit = this.synchronized {
    msg match {
      case EntitySuccessMessage(_, concerning, _, _) if todo.contains(concerning) =>
        todo -= concerning
        update()
      case EntityFailureMessage(_, concerning, _, _, _) if todo.contains(concerning) =>
        todo -= concerning
        update()
      case _ =>
    }
  }
}
