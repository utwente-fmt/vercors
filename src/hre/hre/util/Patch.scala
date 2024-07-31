package hre.util

import hre.util.Patch.{PatchException, UnappliablePatchException}

import java.nio.file.{Files, Path, Paths}
import scala.annotation.tailrec
import scala.jdk.CollectionConverters._
import scala.util.matching.Regex

case object Patch {
  class PatchException(msg: String) extends Exception(msg)
  object NoPatchException extends PatchException("no patch found")
  object UnappliablePatchException
      extends PatchException("could not apply patch")

  def fromFile(p: Path): Seq[Patch] = parsePatches(Files.readString(p))

  def parsePatches(str: String): Seq[Patch] = {
    val (patch, rest) = parseFirstPatch(str)
    if (rest.trim.isEmpty)
      Seq(patch)
    else
      patch +: parsePatches(rest)
  }

  // At least 7 characters per marker. Turn on DOTALL with (?s) to enable matching of newlines
  val startMarker = "(?s)\n<<<<<<<+\n".r
  val middleMarker = "(?s)\n=======+\n".r
  val endMarker = "(?s)\n>>>>>>>+".r

  def parseFirstPatch(str: String): (Patch, String) = {
    val start = startMarker.findFirstMatchIn(str)
      .getOrElse(throw NoPatchException)
    val startLength = start.end - start.start
    assert(startLength >= 7 + 2)

    val middle = middleMarker.findFirstMatchIn(str)
      .getOrElse(throw new PatchException("no middle marker"))
    val end = endMarker.findFirstMatchIn(str)
      .getOrElse(throw new PatchException("no end marker"))
    val middleLength = middle.end - middle.start
    val endLength = end.end - end.start
    assert(middleLength >= 7 + 2 && endLength >= 7 + 1)

    // Subtract to accomodate for the newlines
    if (
      startLength - 2 != middleLength - 2 || middleLength - 2 != endLength - 1
    )
      throw new PatchException(
        s"length of start, middle and end markers do not match at ${start.start}"
      )

    (
      Patch(
        str.substring(start.end, middle.start),
        str.substring(middle.end, end.start),
      ),
      str.substring(end.end),
    )
  }

  def main(args: Array[String]): Unit = {
    println(fromFile(Paths.get(
      "examples/concepts/veymont/FM2024 - VeyMont/1-TTTmsg/testFiles/1-TTTmsg-patches.txt"
    )))
  }

  def applyAll(patches: Seq[Patch], source: String): String =
    patches.foldLeft(source) { case (source, patch) => patch.apply(source) }
}

case class Patch(target: String, newText: String) {
  lazy val r = Regex.quote(target).r

  final def apply(source: String): String =
    r.findFirstMatchIn(source) match {
      case Some(m) =>
        source.substring(0, m.start) + newText +
          source.substring(m.end, source.length)
      case None => throw UnappliablePatchException
    }
}
