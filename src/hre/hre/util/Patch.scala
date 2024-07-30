package hre.util

import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters._
import scala.util.matching.Regex

case object Patch {
  def fromFile(p: Path): Either[String, Seq[Patch]] =
    parsePatches(Files.readString(p))

  def parsePatches(str: String): Either[String, Seq[Patch]] =
    parseFirstPatch(str) match {
      case Left(err) => Left(err)
      case Right((patch, rest)) if rest.trim.isEmpty => Right(Seq(patch))
      case Right((patch, rest)) =>
        parsePatches(rest).map { patches => patch +: patches }
    }

  // At least 7 characters per marker. Turn on DOTALL to enable matching of newlines
  val startMarker = "(?s)<<<<<<<+\n".r
  val middleMarker = "(?s)\n=======+\n".r
  val endMarker = "(?s)\n>>>>>>>+".r

  def parseFirstPatch(str: String): Either[String, (Patch, String)] = {
    val start = startMarker.findFirstMatchIn(str)
      .getOrElse(return Left("no start marker"))
    val startLength = start.end - start.start
    assert(startLength >= 7 + 1)

    val middle = middleMarker.findFirstMatchIn(str)
      .getOrElse(return Left("no middle marker"))
    val end = endMarker.findFirstMatchIn(str)
      .getOrElse(return Left("no end marker"))
    val middleLength = middle.end - middle.start
    val endLength = end.end - end.start
    assert(middleLength >= 7 + 2 && endLength >= 7 + 1)

    // Subtract to accomodate for the newlines
    if (
      startLength - 1 != middleLength - 2 || middleLength - 2 != endLength - 1
    )
      return Left("length of start, middle and end markers do not match")

    Right((
      Patch(
        str.substring(start.end, middle.start),
        str.substring(middle.end, end.start),
      ),
      str.substring(end.end),
    ))
  }

  def main(args: Array[String]): Unit = {
    println(fromFile(Paths.get(
      "examples/concepts/veymont/FM2024 - VeyMont/1-TTTmsg/testFiles/1-TTTmsg-patches.txt"
    )))
  }
}

case class Patch(target: String, newText: String) {
  lazy val r = Regex.quote(target).r
  def apply(source: String): String =
    r.findFirstMatchIn(source) match {
      case Some(m) =>
        apply(
          source.substring(m.start) + newText +
            source.substring(m.end, source.length)
        )
      case None => source
    }
}
