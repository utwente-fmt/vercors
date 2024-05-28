package hre.perf

import com.google.perftools.profiles._

import scala.collection.mutable

class ProfileBuilder {
  private val stringIndex = mutable.Map[String, Long]("" -> 0)
  private val stringTable = mutable.ArrayBuffer[String]("")

  def str(s: String): Long =
    stringIndex.getOrElseUpdate(
      s, {
        stringTable += s
        stringIndex.size
      },
    )

  def finishStringTable(): Seq[String] = stringTable.toIndexedSeq

  private val locationIndex = mutable.Map[String, Long]()
  private val locationTable = mutable.ArrayBuffer[Location]()
  private val functionTable = mutable.ArrayBuffer[Function]()

  def loc(s: String): Long =
    locationIndex.getOrElseUpdate(
      s, {
        val id = functionTable.size + 1
        functionTable += Function(id = id, name = str(s))
        locationTable += Location(id = id, line = Seq(Line(functionId = id)))
        id
      },
    )

  def finishLocationTable(): Seq[Location] = locationTable.toIndexedSeq
  def finishFunctionTable(): Seq[Function] = functionTable.toIndexedSeq

}
