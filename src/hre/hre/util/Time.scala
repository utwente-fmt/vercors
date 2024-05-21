package hre.util

import java.time.{Duration, Instant, LocalDateTime, ZoneId}
import java.time.format.DateTimeFormatter

object Time {
  def formatDuration(duration: Duration): String =
    f"${duration.toHoursPart}%02d:${duration.toMinutesPart}%02d:${duration.toSecondsPart}%02d"

  def formatTime(): String = formatTime(java.time.Instant.now())

  def formatTime(when: Instant): String = {
    val formatter = DateTimeFormatter.ofPattern("HH:mm:ss").withZone(ZoneId.systemDefault())
    formatter.format(when)
  }
}
