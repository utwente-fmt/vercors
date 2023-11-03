package vct.col.origin

import com.typesafe.scalalogging.Logger
import hre.io.Readable
import vct.col.origin.Origin.{BOLD_HR, HR}

import java.io.{Reader, StringReader}
import java.nio.file.Paths
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

sealed trait Name
object Name {
  case class Required(name: String)
  case class Preferred(parts: Seq[String])
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
}

case class PreferredName(preferredName: Seq[String]) extends OriginContent
case class RequiredName(requiredName: String) extends OriginContent
case class ReadableOrigin(readable: Readable) extends OriginContent
case class StartEndLines(startEndLineIdx: (Int, Int)) extends OriginContent
case class OriginCols(cols: Option[(Int, Int)]) extends OriginContent
case class OriginFilename(filename: String) extends OriginContent
case class InlineBipContext(bipContext: String) extends OriginContent

/**
 * A sequence of OriginContents. This sequence can be mutated (add, remove, replace) for convenience.
* @param originContents The known origin contents at the time of Origin creation. Can be empty for a new Origin.
 */
case class Origin(originContents: Seq[OriginContent]) extends Blame[VerificationFailure] {
  def find[T <: OriginContent](implicit tag: ClassTag[T]): Option[T] =
    originContents.collectFirst {
      case t: T => t
    }

  def get[T <: OriginContent](implicit tag: ClassTag[T]): T = find[T].get

  def consume[T <: OriginContent](implicit tag: ClassTag[T]): Option[(T, Origin)] = {
    val (left, right) = originContents.span(!_.isInstanceOf[T])
    right.headOption.map(oc => (oc.asInstanceOf[T], Origin(left ++ right.tail)))
  }

  def withContent(content: OriginContent): Origin =
    Origin(content +: originContents)

  def deleteContent[T <: OriginContent](implicit tag: ClassTag[T]): Origin =
    Origin(originContents.flatMap {
      case _: T => Nil
      case other => Seq(other)
    })

  def replaceContent[T <: OriginContent](content: T)(implicit tag: ClassTag[T]): Origin =
    deleteContent[T].withContent(content)

  def addPrefName(name: String): Origin =
    withContent(PreferredName(name))

  def replacePrefName(name: String): Origin =
    replaceContent[PreferredName](PreferredName(name))

  def replaceContext(name: String): Origin =
    replaceContent[Context](Context(name))

  def addFilename(filename: String): Origin =
    withContent(OriginFilename(filename))

  def addReqName(name: String): Origin =
    withContent(RequiredName(name))

  def addFormalName(name: String): Origin =
    withContent(FormalName(name))

  def addContext(ctx: String): Origin =
    withContent(Context(ctx))

  def addShortPosition(shortP: String): Origin =
    withContent(ShortPosition(shortP))

  def addInlineContext(inCtx: String): Origin =
    withContent(InlineContext(inCtx))

  def addInlineBipContext(bipCtx: String): Origin =
    withContent(InlineBipContext(bipCtx))

  def addReadableOrigin(readable: Readable): Origin =
    withContent(ReadableOrigin(readable))

  def addStartEndLines(startIdx: Int, endIdx: Int): Origin =
    withContent(StartEndLines(startIdx, endIdx))

  def addOriginCols(cols: Option[(Int, Int)]): Origin =
    withContent(OriginCols(cols))

  def findReadable: Option[ReadableOrigin] = find[ReadableOrigin]

  def getContext: Option[Context] =
    find[Context]
      .orElse(Try {
        Context(InputOrigin.contextLines(
          get[ReadableOrigin].readable,
          get[StartEndLines].startEndLineIdx._1,
          get[StartEndLines].startEndLineIdx._2,
          get[OriginCols].cols,
        ))
      }.toOption)

  def getPreferredName: Option[String] = find[PreferredName].map(_.preferredName)

  def getPreferredNameOrElse(name: String = "unknown"): String =
    getPreferredName.getOrElse(name)

  def getInlineContext: Option[InlineContext] = find[InlineContext]

  def getInlineContextOrElse(ctx: String = "[unknown inline context]"): String =
    getInlineContext.getOrElse(InlineContext(ctx)).inlineContext

  def getInlineBipContext: Option[InlineBipContext] =
    find[InlineBipContext]

  def getFilename: Option[OriginFilename] =
    find[OriginFilename]

  def getShortPosition: Option[ShortPosition] =
    find[ShortPosition]

  def getShortPositionOrElse(shortPos: String = "[unknown position]"): String =
    getShortPosition.getOrElse(ShortPosition(shortPos)).shortPosition

  def getStartEndLines: Option[StartEndLines] =
    find[StartEndLines]

  def getOriginCols: Option[OriginCols] =
    find[OriginCols]

  def bareMessageInContext(message: String): String = {
    val contextMessage = getContext match {
      case Some(value) => value.context.strip()
      case None => "[unknown context]"
    }

    contextMessage + "\n" + HR + message + "\n"
  }

  def messageInContext(message: String): String =
    BOLD_HR + bareMessageInContext(message) + BOLD_HR

  override def blame(error: VerificationFailure): Unit = {
    Logger("vct").error(error.toString)
  }
}

object InputOrigin {
  val CONTEXT = 2
  val LINE_NUMBER_WIDTH = 5
  val MAX_INLINE_CONTEXT_WIDTH = 30
  val INLINE_CONTEXT_ELLIPSIS = " ... "

  def contextLines(readable: Readable, unsafeStartLineIdx: Int, unsafeEndLineIdx: Int, unsafeCols: Option[(Int, Int)]): String = {
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

    val startLineIdx = clamp(unsafeStartLineIdx)
    val endLineIdx = clamp(unsafeEndLineIdx)
    val cols = unsafeCols.map {
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

  def inlineContext(readable: Readable, unsafeStartLineIdx: Int, unsafeEndLineIdx: Int, unsafeCols: Option[(Int, Int)]): String =
    readable.readLines().slice(unsafeStartLineIdx, unsafeEndLineIdx+1) match {
      case Nil => "(empty source region)"
      case line +: Nil => unsafeCols match {
        case None => compressInlineText(line)
        case Some((start, end)) => compressInlineText(line.slice(start, end))
      }
      case first +: moreLines =>
        val (context, last) = (moreLines.init, moreLines.last)
        unsafeCols match {
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




