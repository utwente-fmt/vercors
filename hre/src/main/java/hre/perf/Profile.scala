package hre.perf

import com.google.perftools
import com.google.perftools.profiles.{Sample, ValueType}

import java.io.FileOutputStream
import java.util.zip.GZIPOutputStream
import scala.collection.mutable

case object Profile {
  val builder = new ProfileBuilder()

  import builder.{loc, str}

  private val epochStartNanos = System.currentTimeMillis() * 1_000_000L

  private var lastProcessUsage = ResourceUsage.getProcess.get
  private var lastChildUsage = ResourceUsage.getAggregateChildren.get

  private val valueTypes = Seq(
    ValueType(str("agg"), str("microseconds")),
    ValueType(str("aggUser"), str("microseconds")),
    ValueType(str("aggSys"), str("microseconds")),
    ValueType(str("selfUser"), str("microseconds")),
    ValueType(str("selfSys"), str("microseconds")),
    ValueType(str("childUser"), str("microseconds")),
    ValueType(str("childSys"), str("microseconds")),
  )

  private val samples = mutable.ArrayBuffer[Sample]()

  def update(stack: Seq[String]): Unit = {
    val processUsage = ResourceUsage.getProcess.get
    val childUsage = ResourceUsage.getAggregateChildren.get

    val deltaProcess = processUsage - lastProcessUsage
    val deltaChild = childUsage - lastChildUsage
    val deltaAgg = deltaProcess + deltaChild

    val locations = stack.reverse.map(loc)

    samples += Sample(
      locationId = locations,
      value = Seq(
        deltaAgg.userTime + deltaAgg.systemTime,
        deltaAgg.userTime,
        deltaAgg.systemTime,
        deltaProcess.userTime,
        deltaProcess.systemTime,
        deltaChild.userTime,
        deltaChild.systemTime
      ),
    )

    lastProcessUsage = processUsage
    lastChildUsage = childUsage
  }

  def finish(): Unit = {
    val result = perftools.profiles.Profile(
      sampleType = valueTypes,
      sample = samples.toIndexedSeq,
      mapping = Nil,
      location = builder.finishLocationTable(),
      function = builder.finishFunctionTable(),
      stringTable = builder.finishStringTable(),
      timeNanos = epochStartNanos,
      defaultSampleType = builder.str("agg"),
    )
    val out = new GZIPOutputStream(new FileOutputStream("profile.pprof.gz"))
    result.writeTo(out)
    out.close()
  }
}
