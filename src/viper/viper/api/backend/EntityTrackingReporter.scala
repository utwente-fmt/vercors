package viper.api.backend

import hre.progress.Progress
import hre.progress.Progress.Phase
import viper.silver.ast.{Member, Program}
import viper.silver.reporter.{EntityFailureMessage, EntitySuccessMessage, Message, Reporter}

import scala.collection.mutable

case class EntityTrackingReporter() extends Reporter {
  override val name: String = "entity_tracking_reporter"

  def withEntities[T](program: Program)(f: => T): T = {
    val members: Seq[Member] = program.functions ++ program.predicates ++ program.methods

    Progress.parStages(members, (m: Member) => m.name)(_ => f)
  }

  override def report(msg: Message): Unit = this.synchronized {
    msg match {
      case EntitySuccessMessage(_, concerning, _, _) =>
        Progress.nextDone(Phase(concerning.name, 1))
      case EntityFailureMessage(_, concerning, _, _, _) =>
        Progress.nextDone(Phase(concerning.name, 1))
      case _ =>
    }
  }
}
