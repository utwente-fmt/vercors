package vct.col.origin

import com.typesafe.scalalogging.Logger
import vct.col.origin.Origin.{BOLD_HR, HR}
import hre.io.Readable
import spray.json.{JsString, JsValue, JsonParser}
import vct.col.ast.Deserialize
import vct.col.origin.RedirectOrigin.StringReadable

import java.io.{Reader, StringReader}
import java.nio.file.Paths
import scala.collection.mutable.ArrayBuffer

case object Origin {
  val BOLD_HR = "======================================\n"
  val HR = "--------------------------------------\n"

  def messagesInContext(messages: Seq[(Origin, String)]): String =
    messages.zipWithIndex.map { case ((origin, message), idx) =>
      origin.context.replaceAll("(^[\r\n]+)|([\r\n]+$)", "") + "\n" + HR +
        s"[${idx + 1}/${messages.size}] $message\n"
    }.mkString(BOLD_HR, HR, BOLD_HR)
}

trait Origin extends Blame[VerificationFailure] {
  def preferredName: String

  def context: String
  def inlineContext: String
  def shortPosition: String

  def messageInContext(message: String): String =
    context match {
      case "" => message
      case context =>
        BOLD_HR + context.strip() + "\n" + HR + message + "\n" + BOLD_HR
    }

  override def blame(error: VerificationFailure): Unit = {
    Logger("vct").error(error.toString)
  }
}

case object DiagnosticOrigin extends Origin {
  override def preferredName: String = "unknown"
  override def context: String = ""
  override def inlineContext: String = ""
  override def shortPosition: String = "unknown"
}

object InputOrigin {
  val CONTEXT = 2
  val LINE_NUMBER_WIDTH = 5
  val MAX_INLINE_CONTEXT_WIDTH = 30
  val INLINE_CONTEXT_ELLIPSIS = " ... "

  def contextLines(
      readable: Readable,
      unsafeStartLineIdx: Int,
      unsafeEndLineIdx: Int,
      unsafeCols: Option[(Int, Int)],
  ): String = {
    // The newline at the end is dropped, so replace it with two spaces as we need to be able to point to the newline character.
    // ANTLR points past the last line when pointing at an EOF immediately following a newline, hence the extra line.
    val lines = readable.readLines().map(_ + "  ") :+ " "

    val clamp = (line: Int) => Math.max(0, Math.min(lines.size - 1, line))
    val clampCol =
      (line: Int, col: Int) =>
        Math.max(0, Math.min(lines(line).length - 1, col))
    val numberedLine =
      (text: String, line: Int) =>
        String.format(
          "%" + f"$LINE_NUMBER_WIDTH" + "d  %s\n",
          Int.box(line + 1),
          text.dropRight(2),
        )
    val replacementDash =
      (c: Char) =>
        c match {
          case '\t' =>
            "\t" // perhaps derive the tab width from terminal information at some point
          case _ => "-"
        }
    val replacementWhitespace =
      (c: Char) =>
        c match {
          case '\t' => "\t"
          case _ => " "
        }

    val startLineIdx = clamp(unsafeStartLineIdx)
    val endLineIdx = clamp(unsafeEndLineIdx)
    val cols = unsafeCols.map { case (startColIdx, endColIdx) =>
      (clampCol(startLineIdx, startColIdx), clampCol(endLineIdx, endColIdx))
    }

    require(startLineIdx <= endLineIdx)
    require(
      startLineIdx != endLineIdx || cols.isEmpty || cols.get._1 <= cols.get._2
    )

    val firstLineIdx = clamp(startLineIdx - CONTEXT)
    val startContextEnd = clamp(startLineIdx + CONTEXT) + 1
    val endContextStart = clamp(endLineIdx - CONTEXT)
    val endContextEnd = Math.min(lines.size - 1, endLineIdx + CONTEXT) + 1

    val result = new StringBuilder

    // Print any context lines before the first line
    for ((line, idx) <- lines.zipWithIndex.slice(firstLineIdx, startLineIdx)) {
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
        if (startColIdx != 0)
          result.append(" ")

        // Print whitespace, but leave room for [ just before the first character
        lines(startLineIdx).take(startColIdx - 1).map(replacementWhitespace)
          .foreach(result.append)
        result.append("[")
        // If [ stands in for a tab, follow it with a tab to align again. This is wrong when the tab normalizes to only one space. ¯\_(ツ)_/¯
        if (lines(startLineIdx)(startColIdx) == '\t')
          result.append('\t')

        if (startLineIdx == endLineIdx) {
          // Print dashes until endColIdx, as the start and end line coincide.
          lines(startLineIdx).slice(startColIdx, endColIdx).map(replacementDash)
            .foreach(result.append)
        } else {
          // If the start and end line are inequal, print dashes until the end of the line.
          lines(startLineIdx).drop(startColIdx).map(replacementDash)
            .foreach(result.append)
        }

        result.append("\n")
    }

    if (startContextEnd < endContextStart) {
      // There are lines between the end of the starting context and the start of the ending context.
      // We have to print an ellipsis between them.

      // Print the tail of the start context
      for (
        (line, idx) <- lines.zipWithIndex.slice(startLineIdx, startContextEnd)
      ) { result.append(numberedLine(line, idx)) }

      // An ellipsis inbetween...
      result.append(" " * LINE_NUMBER_WIDTH)
      result
        .append(f"  ... (${endContextStart - startContextEnd} lines omitted)\n")

      // And the start of the end context + the end line.
      for (
        (line, idx) <- lines.zipWithIndex.slice(endContextStart, endLineIdx + 1)
      ) { result.append(numberedLine(line, idx)) }
    } else {
      // The start context and end context connect, so just print lines up to and including the ending line
      // If the start and end line coincide, this just prints nothing.
      for (
        (line, idx) <- lines.zipWithIndex.slice(startLineIdx, endLineIdx + 1)
      ) { result.append(numberedLine(line, idx)) }
    }

    // Indent for the ending context
    result.append(" " * LINE_NUMBER_WIDTH).append("  ")

    cols match {
      case None =>
        // If we have no column info, just mark the whole line
        lines(endLineIdx).toSeq.map(replacementDash).foreach(result.append)
        result.append("]\n")
      case Some((startColIdx, endColIdx)) =>
        if (startLineIdx == endLineIdx) {
          // When the start and end line coincide, print whitespace before the dashes until the start column
          lines(endLineIdx).take(startColIdx).map(replacementWhitespace)
            .foreach(result.append)
          lines(endLineIdx).slice(startColIdx, endColIdx).map(replacementDash)
            .foreach(result.append)
        } else {
          // When the start and end line are distinct, just fill the line with dashes until the end column.
          lines(endLineIdx).take(endColIdx).map(replacementDash)
            .foreach(result.append)
        }

        result.append("]\n")
    }

    // Finally, we have to print the tail of the end context.
    for (
      (line, idx) <- lines.zipWithIndex.slice(endLineIdx + 1, endContextEnd)
    ) { result.append(numberedLine(line, idx)) }

    result.toString()
  }

  def sanitizeInlineText(text: String): String =
    text.replaceAll(raw"[\t\r\n]", " ").replaceAll(raw"[ ]+", " ").strip()

  def compressInlineText(text: String): String = {
    val sanitized = sanitizeInlineText(text)

    if (sanitized.length > MAX_INLINE_CONTEXT_WIDTH) {
      val len = MAX_INLINE_CONTEXT_WIDTH - INLINE_CONTEXT_ELLIPSIS.length
      val startLen = len / 2
      val endLen = len - startLen

      sanitizeInlineText(sanitized.take(startLen)) + INLINE_CONTEXT_ELLIPSIS +
        sanitizeInlineText(sanitized.takeRight(endLen))
    } else { sanitized }
  }

  def inlineContext(
      readable: Readable,
      unsafeStartLineIdx: Int,
      unsafeEndLineIdx: Int,
      unsafeCols: Option[(Int, Int)],
  ): String =
    readable.readLines().slice(unsafeStartLineIdx, unsafeEndLineIdx + 1) match {
      case Nil => "(empty source region)"
      case line +: Nil =>
        unsafeCols match {
          case None => compressInlineText(line)
          case Some((start, end)) => compressInlineText(line.slice(start, end))
        }
      case first +: moreLines =>
        val (context, last) = (moreLines.init, moreLines.last)
        unsafeCols match {
          case None =>
            compressInlineText((first +: context :+ last).mkString("\n"))
          case Some((start, end)) =>
            compressInlineText(
              (first.drop(start) +: context :+ last.take(end)).mkString("\n")
            )
        }
    }
}

abstract class InputOrigin extends Origin {
  override def preferredName: String = "unknown"
}

case class BlameCollector() extends Blame[VerificationFailure] {
  val errs: ArrayBuffer[VerificationFailure] = ArrayBuffer()

  override def blame(error: VerificationFailure): Unit = errs += error
}

case class ReadableOrigin(
    readable: Readable,
    startLineIdx: Int,
    endLineIdx: Int,
    cols: Option[(Int, Int)],
) extends InputOrigin {
  private def startText: String =
    cols match {
      case Some((startColIdx, _)) =>
        f"${readable.fileName}:${startLineIdx + 1}:${startColIdx + 1}"
      case None => f"${readable.fileName}:${startLineIdx + 1}"
    }

  private def endText: String =
    cols match {
      case Some((_, endColIdx)) => f"${endLineIdx + 1}:${endColIdx + 1}"
      case None => f"${endLineIdx + 1}"
    }

  private def baseFilename: String =
    Paths.get(readable.fileName).getFileName.toString

  override def shortPosition: String =
    cols match {
      case Some((startColIdx, _)) =>
        f"$baseFilename:${startLineIdx + 1}:${startColIdx + 1}"
      case None => f"$baseFilename:${startLineIdx + 1}"
    }

  override def context: String = {
    val atLine = f" At $startText:\n"

    if (readable.isRereadable) {
      atLine + Origin.HR +
        InputOrigin.contextLines(readable, startLineIdx, endLineIdx, cols)
    } else { atLine }
  }

  override def inlineContext: String =
    if (readable.isRereadable)
      InputOrigin.inlineContext(readable, startLineIdx, endLineIdx, cols)
    else
      f"(non-rereadable source ${readable.fileName})"

  override def toString: String = f"$startText - $endText"
}

case class InterpretedOriginVariable(name: String, original: Origin)
    extends InputOrigin {
  override def preferredName: String = name

  override def context: String = original.context

  override def inlineContext: String = original.inlineContext

  override def shortPosition: String = original.shortPosition
}

case class InterpretedOrigin(
    interpreted: Readable,
    startLineIdx: Int,
    endLineIdx: Int,
    cols: Option[(Int, Int)],
    original: Origin,
) extends InputOrigin {
  private def startText: String =
    cols match {
      case Some((startColIdx, _)) =>
        f"${interpreted.fileName}:${startLineIdx + 1}:${startColIdx + 1}"
      case None => f"${interpreted.fileName}:${startLineIdx + 1}"
    }

  private def endText: String =
    cols match {
      case Some((_, endColIdx)) => f"${endLineIdx + 1}:${endColIdx + 1}"
      case None => f"${endLineIdx + 1}"
    }

  override def shortPosition: String = original.shortPosition

  override def context: String = {
    val interpretedAtLine = f" Interpreted at $startText as:\n"

    val interpretedText =
      if (interpreted.isRereadable) {
        interpretedAtLine + Origin.HR +
          InputOrigin.contextLines(interpreted, startLineIdx, endLineIdx, cols)
      } else { interpretedAtLine }

    original.context + Origin.HR + interpretedText
  }

  override def inlineContext: String =
    if (interpreted.isRereadable)
      InputOrigin.inlineContext(interpreted, startLineIdx, endLineIdx, cols)
    else
      f"(non-rereadable source ${interpreted.fileName})"

  override def toString: String =
    f"$startText - $endText interpreted from $original"
}

case class SourceNameOrigin(name: String, inner: Origin) extends Origin {
  override def preferredName: String = name
  override def shortPosition: String = inner.shortPosition
  override def context: String = inner.context
  override def inlineContext: String = inner.inlineContext

  override def toString: String = s"$name at $inner"
}

case object RedirectOrigin {
  case class StringReadable(
      data: String,
      fileName: String = "<unknown filename>",
  ) extends Readable {
    override def isRereadable: Boolean = true

    override protected def getReader: Reader = new StringReader(data)
  }
}

case class RedirectOrigin(
    o: Origin,
    textualOrigin: String,
    startLine: Int,
    endLine: Int,
    cols: Option[(Int, Int)],
) extends Origin {
  override def preferredName: String = o.preferredName

  override def inlineContext: String = transposedOrigin.inlineContext

  override def shortPosition: String = transposedOrigin.shortPosition

  def transposedOrigin: Origin =
    o match {
      case ReadableOrigin(readable, baseStartLine, baseEndLine, baseCols) =>
        val realStartLine = baseStartLine + startLine
        val realEndLine = baseEndLine + endLine
        val c: Option[(Int, Int)] =
          (baseCols, cols) match {
            case (
                  Some((baseStartCol, _)),
                  Some((innerStartCol, innerEndCol)),
                ) =>
              // + 1 because need to account for starting quote that must be skipped
              val realStart =
                (if (startLine == 0)
                   baseStartCol + innerStartCol
                 else
                   innerStartCol) + 1
              val realEnd =
                (if (endLine == 0)
                   baseStartCol + innerEndCol
                 else
                   innerEndCol) + 1
              Some((realStart, realEnd))
            case (Some(baseCols), None) =>
              if (startLine == 0)
                Some(baseCols)
              else
                None
            case (None, cols) => cols
          }
        ReadableOrigin(readable, realStartLine, realEndLine, c)
      case o: Origin =>
        InterpretedOrigin(
          StringReadable(textualOrigin),
          startLine,
          endLine,
          cols,
          o,
        )
    }

  override def context: String = transposedOrigin.context
}

trait PreferredNameOrigin extends Origin {
  def name: String
  def inner: Origin

  def preferredName: String = name
  def shortPosition: String = inner.shortPosition
  def context: String = inner.context
  def inlineContext: String = inner.inlineContext

  override def toString: String = s"$name at $inner"
}

case class LLVMOrigin(deserializeOrigin: Deserialize.Origin) extends Origin {
  private val parsedOrigin: Option[Map[String, JsValue]] =
    deserializeOrigin.stringOrigin match {
      case string => Some(JsonParser(string).asJsObject().fields)
    }

  def fileName: String = deserializeOrigin.fileName

  override def preferredName: String =
    parsedOrigin match {
      case Some(o) =>
        o.get("preferredName") match {
          case Some(JsString(jsString)) => jsString
          case _ => deserializeOrigin.preferredName
        }
      case None => deserializeOrigin.preferredName
    }

  def contextFragment: String =
    parsedOrigin match {
      case Some(o) =>
        o.get("context") match {
          case Some(JsString(jsString)) => jsString
          case _ => deserializeOrigin.context
        }
      case None => deserializeOrigin.context
    }

  override def context: String = {
    val atLine = f" At $shortPosition:\n"
    if (contextFragment == inlineContext) {
      atLine + Origin.HR + contextFragment
    } else if (contextFragment.contains(inlineContext)) {
      atLine + Origin.HR + markedInlineContext
    } else { deserializeOrigin.context }
  }

  def markedInlineContext: String = {
    val startIndex = contextFragment.indexOf(inlineContext)
    val endIndex = startIndex + inlineContext.length

    val startRowCol = indexToRowCol(contextFragment, startIndex)
    val endRowCol = indexToRowCol(contextFragment, endIndex)

    val lines = contextFragment.split('\n')
    if (startRowCol._1 == endRowCol._1) { // origin only covers (part of) a single row
      val highlight =
        " " * (startRowCol._2) + "[" + ("-" * (inlineContext.length - 2))
          .mkString + "]"
      (lines.slice(0, startRowCol._1) ++ Seq(highlight) ++
        Seq(lines(startRowCol._1)) ++ Seq(highlight) ++
        lines.slice(startRowCol._1 + 1, lines.length)).mkString("\n")
    } else { // origin spans multiple lines, just mark the lines
      val highlight =
        "[" + ("-" * (lines.map(s => s.length).max - 2)).mkString + "]"
      (lines.slice(0, startRowCol._1) ++ Seq(highlight) ++
        lines.slice(startRowCol._1, endRowCol._1 + 1) ++ Seq(highlight) ++
        lines.slice(endRowCol._1 + 1, lines.length)).mkString("\n")
    }
  }

  // assumes no tabs
  def indexToRowCol(fragment: String, index: Int): (Int, Int) = {
    val row = fragment.substring(0, index).count(c => c == '\n')
    val col = fragment.substring(0, index).split('\n').last.length
    (row, col)
  }

  override def inlineContext: String =
    parsedOrigin match {
      case Some(o) =>
        o.get("inlineContext") match {
          case Some(JsString(jsString)) => jsString
          case _ => deserializeOrigin.inlineContext
        }
      case None => deserializeOrigin.inlineContext
    }

  override def shortPosition: String =
    parsedOrigin match {
      case Some(o) =>
        o.get("shortPosition") match {
          case Some(JsString(jsString)) => jsString
          case _ => deserializeOrigin.shortPosition
        }
      case None => deserializeOrigin.shortPosition
    }
}
