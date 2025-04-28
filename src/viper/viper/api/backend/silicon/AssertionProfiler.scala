package viper.api.backend.silicon

import vct.col.origin.{
  Origin,
  OriginFilename,
  PositionRange,
  ReadableOrigin,
  Source,
}
import viper.silicon.logger.records.data.ProverAssertRecord
import viper.silicon.state.terms.Term
import hre.io.Writeable
import viper.api.backend.silicon.AssertionProfiler.ResourceCounter

import java.io.PrintWriter
import scala.collection.mutable

object AssertionProfiler {
  private class ResourceCounter {
    var assertions: Int = 0
    var totalResources: Long = 0L
    var minResources: Long = Long.MaxValue
    var maxResources: Long = Long.MinValue
    var missingStatistics: Boolean = false

    def update(statistics: Option[Map[String, String]]): Unit = {
      assertions += 1
      statistics match {
        case None => invalidate()
        case Some(statistics) =>
          statistics.get("rlimit-count-delta") match {
            case None => invalidate()
            case Some(resources) =>
              resources.toLongOption match {
                case None => invalidate()
                case Some(resources) =>
                  totalResources += resources
                  minResources = minResources.min(resources)
                  maxResources = maxResources.max(resources)
              }
          }
      }
    }
    def invalidate(): Unit = { missingStatistics = true }

    def merge(other: ResourceCounter): ResourceCounter = {
      val newRes = new ResourceCounter()

      newRes.assertions = assertions + other.assertions
      newRes.totalResources = totalResources + totalResources
      newRes.minResources = minResources.min(minResources)
      newRes.maxResources = maxResources.max(maxResources)
      newRes.missingStatistics = missingStatistics || missingStatistics
      newRes
    }
  }
}

class AssertionProfiler {
  private trait OriginOrInternal
  private case class IsOrigin(origin: Origin) extends OriginOrInternal
  private case class IsInternal(term: Term) extends OriginOrInternal

  private val resourceCounts
      : mutable.Map[(Source, PositionRange), ResourceCounter] = mutable
    .HashMap()
  private val dependencies: mutable.Map[Origin, Set[OriginOrInternal]] = mutable
    .HashMap()

  def addAssertionLog(record: ProverAssertRecord): Unit = {
    Util.getOrigin(record.value) match {
      case Some(origin) =>
        val source = origin.findAll[Source].lastOption
        val range = origin.findAll[PositionRange].lastOption
        (source, range) match {
          case (None, _) | (_, None) =>
          case (Some(source), Some(range)) =>
            resourceCounts
              .getOrElseUpdate((source, range), new ResourceCounter())
              .update(record.statistics)
        }
//        dependencies.updateWith(origin) {
//          case None => record.dependencies.map(Util.getOrigin(_))
//        }
      // Ignore if we can't trace it back
      case None =>
    }
  }

  def mergeInto(profiler: AssertionProfiler): Unit = {
    resourceCounts foreach { case (o, res) =>
      profiler.resourceCounts.updateWith(o) {
        case None => Some(res)
        case Some(other) => Some(res.merge(other))
      }
    }
  }

  def outputLog(file: Writeable): Unit = {
    file.write { f =>
      val writer = new PrintWriter(f)
      writer.println("AssertionProfile v0.1")
      writer.println("Resource Counts:")
      resourceCounts foreach {
        case (
              (source, PositionRange(startLineIdx, endLineIdx, startEndColIdx)),
              res,
            ) =>
          val location =
            source match {
              case OriginFilename(filename) =>
                Some(
                  s"$filename:$startLineIdx:${startEndColIdx.map(_._1.toString)
                      .getOrElse("-")}\n$filename:$endLineIdx:${startEndColIdx
                      .map(_._2.toString).getOrElse("-")}"
                )
              case ReadableOrigin(readable) =>
                Some(
                  s"${readable.fileName}:$startLineIdx:${startEndColIdx.map(_._1.toString)
                      .getOrElse("-")}\n${readable.fileName}:$endLineIdx:${startEndColIdx
                      .map(_._2.toString).getOrElse("-")}"
                )
              case _ => None
            }
          location match {
            case Some(location) =>
              writer.println(location)
              if (res.missingStatistics) {
                writer.println(s"Only Assertions: ${res.assertions}")
              } else {
                writer.println(s"Assertions: ${res.assertions}")
                writer.println(s"Total: ${res.totalResources}")
                writer.println(s"Min: ${res.minResources}")
                writer.println(s"Max: ${res.maxResources}")
              }
            case None =>
          }
      }
    }
  }
}
