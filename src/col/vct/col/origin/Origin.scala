package vct.col.origin

import com.typesafe.scalalogging.Logger
import hre.io.{LiteralReadable, Readable}
import vct.result.HasContext
import vct.result.Message.HR

import java.io.{Reader, StringReader}
import java.nio.file.Paths
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag
import scala.util.Try

case object Origin {

}

/***
 * This trait is used to box information about Origins in a structured manner.
 */
trait OriginContent {
  /**
   * Indicates the preferred name or required name for this node. The parts of a preferred name can be joined as
   * desired for the kind of declaration.
   */
  def name(tail: Origin): Option[Name] = None

  /**
   * The reason that this node exists, typeset in a user-friendly way. The contexts are stacked, so context (i)
   * can say something about (i+1). For example: Seq("interpreted c source", "Interpreted from:", "c source"), or
   * Seq("check_sat method for:", "pvl source"). For this method the messages can be multiline, but should not have
   * a trailing or preceding newline.
   */
  def context(tail: Origin): Option[Seq[String]] = None

  /**
   * Should have the same shape as the context method, but instead provides a sequence of breadcrumbs, e.g.:
   * Seq("check_sat", "void test() { }")
   * The part must not contain newlines. Consumers of the inline context should make sure to shorten breadcrumbs and
   * add an ellipsis where approriate, whereas the definition should not try to shorten the breadcrumb (within reason).
   */
  def inlineContext(tail: Origin): Option[Seq[String]] = None

  /**
   * Short description of the source position indicated by this origin. This should refer to the oldest cq original
   * position; not the most recent. If no OriginContent indicates a shortPosition, it should be assumed the node
   * is generated apart from any source input.
   */
  def shortPosition(tail: Origin): Option[String] = None
}

/**
 * Leaf name provider, should consider modifiers like NamePrefix that follow it.
 */
trait NameStrategy extends OriginContent

case class SimpleSourceName(name: String) extends NameStrategy {
  override def name(tail: Origin): Option[Name] =
    Some(Name.Preferred(
      tail.span[NameStrategy]._1.originContents.collect {
        case NamePrefix(prefix) => prefix
      }.reverse :+ name
    ))
}

case class PreferredName(preferredName: Seq[String]) extends NameStrategy {
  override def name(tail: Origin): Option[Name] =
    Some(Name.Preferred(
      tail.span[NameStrategy]._1.originContents.collect {
        case NamePrefix(prefix) => prefix
      }.reverse ++ preferredName
    ))
}

case class NamePrefix(prefix: String) extends OriginContent

case class RequiredName(requiredName: String) extends NameStrategy {
  override def name(tail: Origin): Option[Name] = Some(Name.Required(requiredName))
}

object SourceName {
  def stringToName(name: String): Name = Name.Preferred(
    if (name.forall(c => !c.isLetter || c.isUpper)) name.split("[_]+").toIndexedSeq
    else name.split("[_]+").toIndexedSeq.flatMap(splitNameRec))

  private def splitNameRec(str: String): Seq[String] =
    if (str.isEmpty) Nil
    else {
      val (left, right) = str.tail.span(_.isLower)
      (str.head.toString + left) +: splitNameRec(right)
    }
}

case class SourceName(name: String) extends NameStrategy {
  override def name(tail: Origin): Option[Name] =
    Some(SourceName.stringToName(name))
}

/**
 * Content that provides a bit of context here. By default, this assembles further context from the remaining
 * origin. contextHere and inlineContextHere may optionally consume some more contents, otherwise they can just
 * return the tail as is.
 */
trait Context extends OriginContent {
  protected def contextHere(tail: Origin): (String, Origin)
  protected def inlineContextHere(tail: Origin): (String, Origin)
  protected def shortPositionHere(tail: Origin): (String, Origin)

  override def context(tail: Origin): Option[Seq[String]] = {
    val (head, tailAfterContext) = contextHere(tail)
    Some(head +: tailAfterContext.context.getOrElse(Nil))
  }

  override def inlineContext(tail: Origin): Option[Seq[String]] = {
    val (head, tailAfterContext) = inlineContextHere(tail)
    Some(head +: tailAfterContext.inlineContext.getOrElse(Nil))
  }

  override def shortPosition(tail: Origin): Option[String] = {
    val (head, tailAfterPosition) = shortPositionHere(tail)
    Some(tailAfterPosition.shortPosition.getOrElse(head))
  }
}

/**
 * One or two lowercase words define the context, inline context and short position.
 * For more extended explanations you should implement a case of Context yourself.
 */
case class LabelContext(label: String) extends Context {
  override protected def contextHere(tail: Origin): (String, Origin) = (s"At $label:", tail)
  override protected def inlineContextHere(tail: Origin): (String, Origin) = (label, tail)
  override protected def shortPositionHere(tail: Origin): (String, Origin) = (label, tail)
}

trait Source extends Context {
  def positionContext(position: PositionRange): String
  def inlinePositionContext(position: PositionRange): String
  protected def genericContext: String
  protected def genericInlineContext: String
  def genericShortPosition: String

  override protected def contextHere(tail: Origin): (String, Origin) = (genericContext, tail)
  override protected def inlineContextHere(tail: Origin): (String, Origin) = (genericInlineContext, tail)
  override protected def shortPositionHere(tail: Origin): (String, Origin) = (genericShortPosition, tail)
}

case class ReadableOrigin(readable: Readable) extends Source {
  def positionContext(position: PositionRange): String =
    genericContext + "\n" + HR + InputOrigin.contextLines(readable, position)
  def inlinePositionContext(position: PositionRange): String = InputOrigin.inlineContext(readable, position)
  override def genericContext: String = s"At ${readable.fileName}"
  override def genericInlineContext: String = s"${readable.fileName}"
  override def genericShortPosition: String = s"${readable.fileName}"
}

case class OriginFilename(filename: String) extends Source {
  def positionContext(position: PositionRange): String = genericContext
  def inlinePositionContext(position: PositionRange): String = genericInlineContext
  override def genericContext: String = s"At $filename"
  override def genericInlineContext: String = s"$filename"
  override def genericShortPosition: String = s"$filename"
}

case class InlineBipContext(bipContext: String) extends Source {
  def positionContext(position: PositionRange): String = InputOrigin.contextLines(LiteralReadable("literal", bipContext), position)
  def inlinePositionContext(position: PositionRange): String = InputOrigin.inlineContext(LiteralReadable("literal", bipContext), position)
  override def genericContext: String = s"From literal"
  override def genericInlineContext: String = "literal"
  override def genericShortPosition: String = "literal"
}

case class PositionRange(startLineIdx: Int, endLineIdx: Int, startEndColIdx: Option[(Int, Int)]) extends Context {
  def onlyPositionText: String =
    startEndColIdx match {
      case Some((startColIdx, endColIdx)) =>
        s":${startLineIdx+1}:${startColIdx+1}-:${endLineIdx+1}:${endColIdx+1}"
      case None =>
        s":${startLineIdx+1}-:${endLineIdx+1}"
    }

  override protected def contextHere(tail: Origin): (String, Origin) =
    tail.spanFind[Source] match {
      case (_, Some((source, tail))) => (source.positionContext(this), tail)
      case (_, None) => ("At broken position", tail)
    }

  override protected def inlineContextHere(tail: Origin): (String, Origin) =
    tail.spanFind[Source] match {
      case (_, Some((source, tail))) => (source.inlinePositionContext(this), tail)
      case (_, None) => ("broken position", tail)
    }

  override protected def shortPositionHere(tail: Origin): (String, Origin) =
    tail.spanFind[Source] match {
      case (_, Some((source, tail))) => (s"${source.genericShortPosition}$onlyPositionText", tail)
      case (_, None) => ("broken position", tail)
    }
}

/**
 * A sequence of OriginContents. This sequence can be mutated (add, remove, replace) for convenience.
* @param originContents The known origin contents at the time of Origin creation. Can be empty for a new Origin.
 */
final case class Origin(originContents: Seq[OriginContent]) extends Blame[VerificationFailure] with HasContext {
  def tail: Origin = Origin(originContents.tail)

  def find[T <: OriginContent](implicit tag: ClassTag[T]): Option[T] =
    originContents.collectFirst {
      case t: T => t
    }

  def findAll[T <: OriginContent](implicit tag: ClassTag[T]): Seq[T] =
    originContents.collect {
      case t: T => t
    }

  def span[T <: OriginContent](implicit tag: ClassTag[T]): (Origin, Origin) = {
    val (left, right) = originContents.span {
      case t: T => false
      case _ => true
    }

    (Origin(left), Origin(right))
  }

  def spanFind[T <: OriginContent](implicit tag: ClassTag[T]): (Origin, Option[(T, Origin)]) = {
    val (left, Origin(right)) = span[T]

    right.headOption match {
      case Some(t: T) => (left, Some((t, Origin(right.tail))))
      case Some(_) => ???
      case None => (left, None)
    }
  }

  def get[T <: OriginContent](implicit tag: ClassTag[T]): T = find[T].get

  def withContent(content: OriginContent): Origin =
    Origin(content +: originContents)

  def where(
    context: String = null,
    name: String = null,
    prefix: String = null,
  ): Origin = Origin(
    Option(context).map(LabelContext).toSeq ++
      Option(name).map(name => PreferredName(Seq(name))).toSeq ++
      Option(prefix).map(NamePrefix).to(Seq) ++
      originContents
  )

  /**
   * Do not use to indicate a preferred name. This is to indicate the formal
   * source name in the input. Use where(name = ...) instead.
   */
  def sourceName(name: String): Origin =
    withContent(SourceName(name))

//  def addStartEndLines(startIdx: Int, endIdx: Int): Origin =
//    withContent(StartEndLines(startIdx, endIdx))
//
//  def addOriginCols(cols: Option[(Int, Int)]): Origin =
//    withContent(OriginCols(cols))

  def getPreferredName: Option[Name] =
    originContents.headOption.flatMap(_.name(tail).orElse(tail.getPreferredName))

  def getPreferredNameOrElse(name: Seq[String] = Seq("unknown")): Name =
    getPreferredName.getOrElse(Name.Preferred(name))

  def context: Option[Seq[String]] =
    // Use the first content to provide context (if it exists), else use the tail.
    originContents.headOption.flatMap(_.context(tail).orElse(tail.context))

  def contextText: String =
    context.map(_.mkString("\n" + HR)).getOrElse("[unknown context]")

  def inlineContext: Option[Seq[String]] =
    originContents.headOption.flatMap(_.inlineContext(tail).orElse(tail.inlineContext))

  def inlineContextText: String =
    inlineContext.map(_.mkString(" > ")).getOrElse("[unknown context]")

  def shortPosition: Option[String] =
    originContents.headOption.flatMap(_.shortPosition(tail).orElse(tail.shortPosition))

  def shortPositionText: String =
    shortPosition.getOrElse("[unknown position]")

  override def blame(error: VerificationFailure): Unit = {
    Logger("vct").error(error.toString)
  }
}

object InputOrigin {
  val CONTEXT = 2
  val LINE_NUMBER_WIDTH = 5
  val MAX_INLINE_CONTEXT_WIDTH = 30
  val INLINE_CONTEXT_ELLIPSIS = " ... "

  def contextLines(readable: Readable, position: PositionRange): String = {
    // The newline at the end is dropped, so replace it with two spaces as we need to be able to point to the newline character.
    // ANTLR points past the last line when pointing at an EOF immediately following a newline, hence the extra line.
    val lines = readable.readLines().map(_ + "  ") :+ " "

    val clamp = (line: Int) => Math.max(0, Math.min(lines.size-1, line))
    val clampCol = (line: Int, col: Int) => Math.max(0, Math.min(lines(line).length-1, col))
    val numberedLine = (text: String, line: Int) => String.format("%" + f"$LINE_NUMBER_WIDTH" + "d  %s\n", Int.box(line+1), text.dropRight(2))
    val replacementDash = (c: Char) => c match {
      case '\t' => "\t" // perhaps derive the tab width from terminal information at some point
      case _ => "-"
    }
    val replacementWhitespace = (c: Char) => c match {
      case '\t' => "\t"
      case _ => " "
    }

    val startLineIdx = clamp(position.startLineIdx)
    val endLineIdx = clamp(position.endLineIdx)
    val cols = position.startEndColIdx.map {
      case (startColIdx, endColIdx) => (clampCol(startLineIdx, startColIdx), clampCol(endLineIdx, endColIdx))
    }

    require(startLineIdx <= endLineIdx)
    require(startLineIdx != endLineIdx || cols.isEmpty || cols.get._1 <= cols.get._2)

    val firstLineIdx = clamp(startLineIdx - CONTEXT)
    val startContextEnd = clamp(startLineIdx + CONTEXT) + 1
    val endContextStart = clamp(endLineIdx - CONTEXT)
    val endContextEnd = Math.min(lines.size-1, endLineIdx + CONTEXT) + 1

    val result = new StringBuilder

    // Print any context lines before the first line
    for((line, idx) <- lines.zipWithIndex.slice(firstLineIdx, startLineIdx)) {
      result.append(numberedLine(line, idx))
    }

    // Just before the first line: indent with one space too little to make room for [ if we start at the first character.
    result.append(" " * LINE_NUMBER_WIDTH).append(" ")

    cols match {
      case None =>
        // If we have no column info, just mark the whole line
        result.append("[")
        lines(startLineIdx).toSeq.map(replacementDash).foreach(result.append)
        result.append("\n")
      case Some((startColIdx, endColIdx)) =>
        // Leave room for [ if we start at the first character
        if(startColIdx != 0) result.append(" ")

        // Print whitespace, but leave room for [ just before the first character
        lines(startLineIdx).take(startColIdx-1).map(replacementWhitespace).foreach(result.append)
        result.append("[")
        // If [ stands in for a tab, follow it with a tab to align again. This is wrong when the tab normalizes to only one space. ¯\_(ツ)_/¯
        if(lines(startLineIdx)(startColIdx) == '\t') result.append('\t')

        if(startLineIdx == endLineIdx) {
          // Print dashes until endColIdx, as the start and end line coincide.
          lines(startLineIdx).slice(startColIdx, endColIdx).map(replacementDash).foreach(result.append)
        } else {
          // If the start and end line are inequal, print dashes until the end of the line.
          lines(startLineIdx).drop(startColIdx).map(replacementDash).foreach(result.append)
        }

        result.append("\n")
    }

    if(startContextEnd < endContextStart) {
      // There are lines between the end of the starting context and the start of the ending context.
      // We have to print an ellipsis between them.

      // Print the tail of the start context
      for((line, idx) <- lines.zipWithIndex.slice(startLineIdx, startContextEnd)) {
        result.append(numberedLine(line, idx))
      }

      // An ellipsis inbetween...
      result.append(" " * LINE_NUMBER_WIDTH)
      result.append(f"  ... (${endContextStart - startContextEnd} lines omitted)\n")

      // And the start of the end context + the end line.
      for((line, idx) <- lines.zipWithIndex.slice(endContextStart, endLineIdx+1)) {
        result.append(numberedLine(line, idx))
      }
    } else {
      // The start context and end context connect, so just print lines up to and including the ending line
      // If the start and end line coincide, this just prints nothing.
      for((line, idx) <- lines.zipWithIndex.slice(startLineIdx, endLineIdx+1)) {
        result.append(numberedLine(line, idx))
      }
    }

    // Indent for the ending context
    result.append(" " * LINE_NUMBER_WIDTH).append("  ")

    cols match {
      case None =>
        // If we have no column info, just mark the whole line
        lines(endLineIdx).toSeq.map(replacementDash).foreach(result.append)
        result.append("]\n")
      case Some((startColIdx, endColIdx)) =>
        if(startLineIdx == endLineIdx) {
          // When the start and end line coincide, print whitespace before the dashes until the start column
          lines(endLineIdx).take(startColIdx).map(replacementWhitespace).foreach(result.append)
          lines(endLineIdx).slice(startColIdx, endColIdx).map(replacementDash).foreach(result.append)
        } else {
          // When the start and end line are distinct, just fill the line with dashes until the end column.
          lines(endLineIdx).take(endColIdx).map(replacementDash).foreach(result.append)
        }

        result.append("]\n")
    }

    // Finally, we have to print the tail of the end context.
    for((line, idx) <- lines.zipWithIndex.slice(endLineIdx+1, endContextEnd)) {
      result.append(numberedLine(line, idx))
    }

    result.toString().stripTrailing()
  }

  def sanitizeInlineText(text: String): String =
    text.replaceAll(raw"[\t\r\n]", " ").replaceAll(raw"[ ]+", " ").strip()

  def compressInlineText(text: String): String = {
    val sanitized = sanitizeInlineText(text)

    if(sanitized.length > MAX_INLINE_CONTEXT_WIDTH) {
      val len = MAX_INLINE_CONTEXT_WIDTH - INLINE_CONTEXT_ELLIPSIS.length
      val startLen = len / 2
      val endLen = len - startLen

      sanitizeInlineText(sanitized.take(startLen)) +
        INLINE_CONTEXT_ELLIPSIS +
        sanitizeInlineText(sanitized.takeRight(endLen))
    } else {
      sanitized
    }
  }

  def inlineContext(readable: Readable, position: PositionRange): String =
    readable.readLines().slice(position.startLineIdx, position.endLineIdx+1) match {
      case Nil => "(empty source region)"
      case line +: Nil => position.startEndColIdx match {
        case None => compressInlineText(line)
        case Some((start, end)) => compressInlineText(line.slice(start, end))
      }
      case first +: moreLines =>
        val (context, last) = (moreLines.init, moreLines.last)
        position.startEndColIdx match {
          case None => compressInlineText((first +: context :+ last).mkString("\n"))
          case Some((start, end)) => compressInlineText((first.drop(start) +: context :+ last.take(end)).mkString("\n"))
        }
    }
}

case class BlameCollector() extends Blame[VerificationFailure] {
  val errs: ArrayBuffer[VerificationFailure] = ArrayBuffer()

  override def blame(error: VerificationFailure): Unit =
    errs += error
}

case object RedirectOrigin {
  case class StringReadable(data: String, fileName:String="<unknown filename>") extends Readable {
    override def isRereadable: Boolean = true

    override protected def getReader: Reader =
      new StringReader(data)
  }

}




