package hre.perf

import hre.perf.ResourceUsage.Microseconds
import hre.unix.{LibC, RUsage}

object ResourceUsage {
  type Microseconds = Long

  private def get(who: Int): Option[ResourceUsage] = {
    val usage = new RUsage()
    if(LibC.INSTANCE.getrusage(who, usage) != 0) return None

    Some(ResourceUsage(usage.ru_utime.toUsec, usage.ru_stime.toUsec, usage.ru_inblock, usage.ru_oublock, usage.ru_nvcsw, usage.ru_nivcsw))
  }

  def getProcess: Option[ResourceUsage] = get(0)
  def getCallingThread: Option[ResourceUsage] = get(1)
  def getAggregateChildren: Option[ResourceUsage] = get(-1)

  def main(args: Array[String]): Unit = {
    println(System.getProperty("sun.arch.data.model"))
    while(true) {
      println(s"self: ${getProcess.get}")
      println(s"calling thread: ${getCallingThread.get}")
      println(s"children: ${getAggregateChildren.get}")
      println()
      Thread.sleep(1000)
    }
  }

}

case class ResourceUsage(
  userTime: Microseconds,
  systemTime: Microseconds,
  readBlocks: Long,
  writtenBlocks: Long,
  voluntaryContextSwitches: Long,
  involuntaryContextSwitches: Long,
) {
  override def toString: String =
    s"user=${userTime}μs sys=${systemTime}μs in=${readBlocks}blocks out=${writtenBlocks}blocks voluntaryYield=${voluntaryContextSwitches} involuntaryYield=${involuntaryContextSwitches}"

  def applyOp(op: (Long, Long) => Long)(other: ResourceUsage): ResourceUsage = {
    val result = ResourceUsage(
      userTime = op(userTime, other.userTime),
      systemTime = op(systemTime, other.systemTime),
      readBlocks = op(readBlocks, other.readBlocks),
      writtenBlocks = op(writtenBlocks, other.writtenBlocks),
      voluntaryContextSwitches = op(voluntaryContextSwitches, other.voluntaryContextSwitches),
      involuntaryContextSwitches = op(involuntaryContextSwitches, other.involuntaryContextSwitches),
    )
    assert(result.userTime >= 0)
    assert(result.systemTime >= 0)
    assert(result.readBlocks >= 0)
    assert(result.writtenBlocks >= 0)
    assert(result.voluntaryContextSwitches >= 0)
    assert(result.involuntaryContextSwitches >= 0)
    result
  }

  def -(other: ResourceUsage): ResourceUsage = applyOp(_ - _)(other)
  def +(other: ResourceUsage): ResourceUsage = applyOp(_ + _)(other)
}