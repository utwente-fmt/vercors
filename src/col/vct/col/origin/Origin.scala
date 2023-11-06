package vct.col.origin

import com.typesafe.scalalogging.Logger
import hre.io.{LiteralReadable, Readable}
import vct.col.origin.Origin.{BOLD_HR, HR}

import java.io.{Reader, StringReader}
import java.nio.file.Paths
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag
import scala.util.Try

case object Origin {
  val BOLD_HR = "======================================\n"
  val HR      = "--------------------------------------\n"

  def messagesInContext(messages: Seq[(Origin, String)]): String =
    messages.zipWithIndex.map {
      case ((origin, message), idx) =>
        origin.getContext.getOrElse(Context("")).context.replaceAll("(^[\r\n]+)|([\r\n]+$)", "") + "\n" + HR+ s"[${idx+1}/${messages.size}] $message\n"
    }.mkString(BOLD_HR, HR, BOLD_HR)
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

/**
 * Content that provides a bit of context here. By default, this assembles further context from the remaining
 * origin. contextHere and inlineContextHere may optionally consume some more contents, otherwise they can just
 * return the tail as is.
 */
trait Context1 extends OriginContent {
  protected def contextHere(tail: Origin): (String, Origin)
  protected def inlineContextHere(tail: Origin): (String, Origin)

  override def context(tail: Origin): Option[Seq[String]] = {
    val (head, tailAfterContext) = contextHere(tail)
    Some(head +: tailAfterContext.context.getOrElse(Nil))
  }

  override def inlineContext(tail: Origin): Option[Seq[String]] = {
    val (head, tailAfterContext) = inlineContextHere(tail)
    Some(head +: tailAfterContext.inlineContext.getOrElse(Nil))
  }

}

case class LabelContext(label: String) extends Context1 {
  override protected def contextHere(tail: Origin): (String, Origin) = (s"At $label:", tail)
  override protected def inlineContextHere(tail: Origin): (String, Origin) = (label, tail)
}

trait Source extends Context1 {
  def positionContext(position: PositionRange): String
  def inlinePositionContext(position: PositionRange): String
  protected def genericContext: String
  protected def genericInlineContext: String

  override protected def contextHere(tail: Origin): (String, Origin) = (genericContext, tail)
  override protected def inlineContextHere(tail: Origin): (String, Origin) = (genericInlineContext, tail)
}

case class ReadableOrigin(readable: Readable) extends Source {
  def positionContext(position: PositionRange): String = InputOrigin.contextLines(readable, position)
  def inlinePositionContext(position: PositionRange): String = InputOrigin.inlineContext(readable, position)
  override def genericContext: String = s"At ${readable.fileName}"
  override def genericInlineContext: String = s"${readable.fileName}"
}

case class OriginFilename(filename: String) extends Source {
  def positionContext(position: PositionRange): String = genericContext
  def inlinePositionContext(position: PositionRange): String = genericInlineContext
  override def genericContext: String = s"At $filename"
  override def genericInlineContext: String = s"$filename"
}

case class InlineBipContext(bipContext: String) extends Source {
  def positionContext(position: PositionRange): String = InputOrigin.contextLines(LiteralReadable("literal", bipContext), position)
  def inlinePositionContext(position: PositionRange): String = InputOrigin.inlineContext(LiteralReadable("literal", bipContext), position)
  override def genericContext: String = s"From literal"
  override def genericInlineContext: String = "literal"
}

case class PositionRange(startLineIdx: Int, endLineIdx: Int, startEndColIdx: Option[(Int, Int)]) extends Context1 {
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
}

/**
 * A sequence of OriginContents. This sequence can be mutated (add, remove, replace) for convenience.
* @param originContents The known origin contents at the time of Origin creation. Can be empty for a new Origin.
 */
case class Origin(originContents: Seq[OriginContent]) extends Blame[VerificationFailure] {
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

  def consume[T <: OriginContent](implicit tag: ClassTag[T]): Option[(T, Origin)] = {
    val (left, right) = originContents.span(!_.isInstanceOf[T])
    right.headOption.map(oc => (oc.asInstanceOf[T], Origin(left ++ right.tail)))
  }

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

//  def addStartEndLines(startIdx: Int, endIdx: Int): Origin =
//    withContent(StartEndLines(startIdx, endIdx))
//
//  def addOriginCols(cols: Option[(Int, Int)]): Origin =
//    withContent(OriginCols(cols))

  def getPreferredName: Option[String] = find[PreferredName].map(_.preferredName)

  def getPreferredNameOrElse(name: String = "unknown"): String =
    getPreferredName.getOrElse(name)

  def context: Option[Seq[String]] =
    // Use the first content to provide context (if it exists), else use the tail.
    originContents.headOption.flatMap(_.context(tail).orElse(tail.context))

  def contextText: String =
    context.map(_.mkString("\n" + HR)).getOrElse("[unknown context]")

  def inlineContext: Option[Seq[String]] =
    originContents.headOption.flatMap(_.inlineContext(tail).orElse(tail.inlineContext))

  def inlineContextText: String =
    inlineContext.map(_.mkString(" > ")).getOrElse("[unknown context]")

  def bareMessageInContext(message: String): String =
    contextText + "\n" + HR + message

  def messageInContext(message: String): String =
    BOLD_HR + bareMessageInContext(message) + "\n" + BOLD_HR

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

    result.toString()
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

object DiagnosticOrigin extends Origin(Nil)

// decided to include the readable in the origin itself, this is perhaps not necessary?
object UserInputOrigin {
  def apply(readable: Readable,
            startLineIdx: Int, endLineIdx: Int,
            cols: Option[(Int, Int)]): Origin = {

    def startText: String = cols match {
      case Some((startColIdx, _)) => f"${readable.fileName}:${startLineIdx + 1}:${startColIdx + 1}"
      case None => f"${readable.fileName}:${startLineIdx + 1}"
    }

    def baseFilename: String = Paths.get(readable.fileName).getFileName.toString

    def inlineContext: String = {
      if (readable.isRereadable)
        InputOrigin.inlineContext(readable, startLineIdx, endLineIdx, cols)
      else
        f"(non-rereadable source ${readable.fileName})"
    }

    def context: String = {
      val atLine = f" At $startText:\n"

      if (readable.isRereadable) {
        atLine + Origin.HR + InputOrigin.contextLines(readable, startLineIdx, endLineIdx, cols)
      } else {
        atLine
      }
    }

    def shortPosition: String = cols match {
      case Some((startColIdx, _)) => f"$baseFilename:${startLineIdx + 1}:${startColIdx + 1}"
      case None => f"$baseFilename:${startLineIdx + 1}"
    }

    Origin(Seq(ShortPosition(shortPosition), Context(context), InlineContext(inlineContext)))
  }
}

case class BlameCollector() extends Blame[VerificationFailure] {
  val errs: ArrayBuffer[VerificationFailure] = ArrayBuffer()

  override def blame(error: VerificationFailure): Unit =
    errs += error
}

object InterpretedOrigin {
  def apply(interpreted: Readable,
            startLineIdx: Int, endLineIdx: Int,
            cols: Option[(Int, Int)],
            original: Origin): Origin = {

    def startText: String = cols match {
      case Some((startColIdx, _)) => f"${interpreted.fileName}:${startLineIdx + 1}:${startColIdx + 1}"
      case None => f"${interpreted.fileName}:${startLineIdx + 1}"
    }

    def context: Seq[OriginContent] = {
      val interpretedAtLine = f" Interpreted at $startText as:\n"

      val interpretedText = if (interpreted.isRereadable) {
        interpretedAtLine + Origin.HR + InputOrigin.contextLines(interpreted, startLineIdx, endLineIdx, cols)
      } else {
        interpretedAtLine
      }

      original.originContents.map {
        case Context(message) => Context(message + Origin.HR + interpretedText)
        case other => other
      }
    }

    def inlineContext: String =
      if (interpreted.isRereadable)
        InputOrigin.inlineContext(interpreted, startLineIdx, endLineIdx, cols)
      else
        f"(non-rereadable source ${interpreted.fileName})"

    Origin(context :+ InlineContext(inlineContext))
  }


}

case object RedirectOrigin {
  case class StringReadable(data: String, fileName:String="<unknown filename>") extends Readable {
    override def isRereadable: Boolean = true

    override protected def getReader: Reader =
      new StringReader(data)
  }

  def transposeOrigin(o: Origin, textualOrigin: String, startLine: Int, endLine: Int, cols: Option[(Int, Int)]): Origin
  = o.originContents.collectFirst {
    case ReadableOrigin(readable) =>
      val startEndLine = o.getStartEndLines
      val realStartLine = startEndLine.get.startEndLineIdx._1 + startLine
      val realEndLine = startEndLine.get.startEndLineIdx._2 + endLine
      val c: Option[(Int, Int)] = (o.getOriginCols.get.cols, cols) match {
        case (Some((baseStartCol, _)), Some((innerStartCol, innerEndCol))) =>
          // + 1 because need to account for starting quote that must be skipped
          val realStart = (if (startLine == 0) baseStartCol + innerStartCol else innerStartCol) + 1
          val realEnd = (if (endLine == 0) baseStartCol + innerEndCol else innerEndCol) + 1
          Some((realStart, realEnd))
        case (Some(baseCols), None) => if (startLine == 0) Some(baseCols) else None
        case (None, cols) => cols
      }
      Origin(Seq(ReadableOrigin(readable), StartEndLines(realStartLine, realEndLine), OriginCols(c)))
    case _ =>
      InterpretedOrigin(StringReadable(textualOrigin), startLine, endLine, cols, o)
  }.get

}




