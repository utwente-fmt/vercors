package hre.perf

import ch.qos.logback.core.property.ResourceExistsPropertyDefiner
import com.google.perftools
import com.google.perftools.profiles.{Sample, ValueType}

import java.io.FileOutputStream
import java.util.zip.GZIPOutputStream
import scala.collection.mutable

case object Profile {
  val builder = new ProfileBuilder()

  import builder.{loc, str}

  private val epochStartNanos = System.currentTimeMillis() * 1_000_000L

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

  def update(stack: Seq[String], ownUsage: ResourceUsage, doUpdateChildUsage: Boolean): Unit = {
    val deltaChild = if(doUpdateChildUsage) {
      val childUsage = ResourceUsage.getAggregateChildren.get
      val deltaChild = childUsage - lastChildUsage
      lastChildUsage = childUsage
      deltaChild
    } else {
      ResourceUsage(0, 0, 0, 0, 0, 0)
    }

    val deltaAgg = deltaChild + ownUsage

    val locations = stack.map(loc)

    samples += Sample(
      locationId = locations,
      value = Seq(
        deltaAgg.userTime + deltaAgg.systemTime,
        deltaAgg.userTime,
        deltaAgg.systemTime,
        ownUsage.userTime,
        ownUsage.systemTime,
        deltaChild.userTime,
        deltaChild.systemTime
      ),
    )
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
