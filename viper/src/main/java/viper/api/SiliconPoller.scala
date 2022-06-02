package viper.api

import com.typesafe.scalalogging.LazyLogging
import vct.col.ast.{Expr, Node, Statement}
import viper.silicon.logger.{NoopSymbLog, SymbExLogger}
import viper.silicon.logger.records.data.{ConsumeRecord, ExecuteRecord, FunctionRecord, MethodRecord, PredicateRecord, ProduceRecord}

import java.util.{Timer, TimerTask}

case class SiliconPoller(interval: Long) extends LazyLogging {
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

  def poll(): Unit = {
    val log = SymbExLogger.currentLog()

    if(log == NoopSymbLog) return

    log.main match {
      case record: FunctionRecord => logger.warn(s"Silicon is verifying function ${record.value.name}")
      case record: MethodRecord => logger.warn(s"Silicon is verifying method ${record.value.name}")
      case record: PredicateRecord => logger.warn(s"Silicon is verifying predicate ${record.value.name}")
    }

    log.log.reverse.foreach {
      case record: ExecuteRecord =>
        val statement = record.value
        val colStatement = statement.info.getUniqueInfo[NodeInfo[Statement[_]]].get.node
        logger.warn(colStatement.o.messageInContext("Silicon is currently executing this"))
        logger.warn(s"Silver statement: $statement")
        return
      case record: ProduceRecord =>
        val resource = record.value
        val colResource = resource.info.getUniqueInfo[NodeInfo[Expr[_]]].get.node
        logger.warn(colResource.o.messageInContext("Silicon is currently exhaling this"))
        logger.warn(s"Silver resource: $resource")
        return
      case _ => // continue
    }

    logger.warn("However, we cannot tell what it's currently doing...")
  }
}