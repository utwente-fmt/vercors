package hre.perf

import com.google.perftools
import com.google.perftools.profiles.{Sample, ValueType}

import java.io.FileOutputStream
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

    val selfUser = processUsage.userTime - lastProcessUsage.userTime
    val selfSys = processUsage.systemTime - lastProcessUsage.systemTime
    val childUser = childUsage.userTime - lastChildUsage.userTime
    val childSys = childUsage.systemTime - lastChildUsage.userTime
    val aggUser = selfUser + childUser
    val aggSys = selfSys + childSys
    val agg = aggUser + aggSys

    val locations = stack.reverse.map(loc)

    samples += Sample(
      locationId = locations,
      value = Seq(agg, aggUser, aggSys, selfUser, selfSys, childUser, childSys),
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
    val out = new FileOutputStream("profile.pprof")
    result.writeTo(out)
    out.close()
  }
}
