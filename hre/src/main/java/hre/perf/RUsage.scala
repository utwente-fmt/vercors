package hre.perf

import hre.perf.RUsage.Microseconds
import hre.unix
import hre.unix.LibC

object RUsage {
  type Microseconds = Long
  type Kilobytes = Long

  private def get(who: Int): Option[RUsage] = {
    val usage = new unix.RUsage()
    if(LibC.INSTANCE.getrusage(who, usage) != 0) return None

    Some(RUsage(usage.ru_utime.toUsec, usage.ru_stime.toUsec, usage.ru_inblock, usage.ru_oublock, usage.ru_nvcsw, usage.ru_nivcsw))
  }

  def getProcess: Option[RUsage] = get(0)
  def getCallingThread: Option[RUsage] = get(1)
  def getAggregateChildren: Option[RUsage] = get(-1)

  def main(args: Array[String]): Unit = {
    while(true) {
      println(s"self: ${getProcess.get}")
      println(s"calling thread: ${getCallingThread.get}")
      println(s"children: ${getAggregateChildren.get}")
      println()
      Thread.sleep(1000)
    }
  }

}

case class RUsage(
  userTime: Microseconds,
  systemTime: Microseconds,
  readBlocks: Long,
  writtenBlocks: Long,
  voluntaryContextSwitches: Long,
  involuntaryContextSwitches: Long,
) {
  override def toString: String =
    s"user=${userTime}μs sys=${systemTime}μs in=${readBlocks}blocks out=${writtenBlocks}blocks voluntaryYield=${voluntaryContextSwitches} involuntaryYield=${involuntaryContextSwitches}"
}