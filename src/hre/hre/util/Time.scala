package hre.util

import com.typesafe.scalalogging.LazyLogging

import java.time.{Duration, Instant, LocalDateTime, ZoneId}
import java.time.format.DateTimeFormatter

object Time extends LazyLogging {
  def formatDuration(duration: Duration): String =
    f"${duration.toHoursPart}%02d:${duration.toMinutesPart}%02d:${duration.toSecondsPart}%02d"

  def formatTime(): String = formatTime(java.time.Instant.now())

  def formatTime(when: Instant): String = {
    val formatter = DateTimeFormatter.ofPattern("HH:mm:ss")
      .withZone(ZoneId.systemDefault())
    formatter.format(when)
  }

  def logTime[T](name: String, f: => T): T = {
    logger.info(s"Start: $name (at ${formatTime()})")
    val start = java.time.Instant.now()
    try { f }
    finally {
      val duration = Duration.between(start, java.time.Instant.now())
      logger.info(
        s"Done: $name (at ${formatTime()}, duration: ${formatDuration(duration)})"
      )
    }
  }
}
