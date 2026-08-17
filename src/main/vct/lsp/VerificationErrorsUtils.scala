package vct.lsp

import org.eclipse.lsp4j.{
  Diagnostic,
  DiagnosticRelatedInformation,
  DiagnosticSeverity,
  Location,
  Position,
  PublishDiagnosticsParams,
  Range,
}
import vct.col.ast.Node
import vct.col.check.CheckError
import vct.col.origin.{Origin, PositionRange, VerificationFailure}
import vct.lsp.LspMessages.showError
import vct.main.stages.HasCheckErrors
import vct.result.VerificationError

import scala.jdk.CollectionConverters._

object VerificationErrorsUtils {

  def sendUnexpectedFailureDiagnostics(
      uri: String,
      failures: Seq[VerificationFailure],
  ): Unit = {

    val diagnostics = failures.flatMap { vf =>
      val mainDiagOpt = vf.originsWithMessages.headOption.flatMap {
        case (origin, _) => originToDiagnostic(origin, vf.inlineDesc)
      }
      val related = vf.originsWithMessages.drop(1)
        .flatMap { case (origin, msg) =>
          originToDiagnostic(origin, msg).map { diag =>
            new DiagnosticRelatedInformation(
              new Location(uri, diag.getRange),
              diag.getMessage.getLeft,
            )
          }
        }

      mainDiagOpt match {
        case Some(mainDiag) =>
          if (related.nonEmpty) {
            mainDiag.setRelatedInformation(related.asJava)
          }
          List(mainDiag)
        case None =>
          showError(
            s"Unhandled verification failure: ${vf.getClass.getSimpleName} – ${vf.inlineDesc}"
          )
          Nil
      }
    }
    MyLanguageServer.client
      .publishDiagnostics(new PublishDiagnosticsParams(uri, diagnostics.asJava))
  }

  private def originToDiagnostic(
      origin: Origin,
      message: String,
  ): Option[Diagnostic] = {
    origin.find[PositionRange].flatMap {
      case PositionRange(startLine, endLine, Some((startCol, endCol))) =>
        Some(new Diagnostic(
          new Range(
            new Position(startLine, startCol),
            new Position(endLine, endCol),
          ),
          message,
          DiagnosticSeverity.Error,
          "VerCors",
        ))
      case PositionRange(startLine, endLine, None) =>
        Some(new Diagnostic(
          new Range(new Position(startLine, 0), new Position(endLine, 0)),
          message,
          DiagnosticSeverity.Error,
          "VerCors",
        ))
    }
  }

  def sendVerificationErrorDiagnostic(
      uri: String,
      err: VerificationError,
  ): Unit =
    err match {
      case vf: VerificationFailure =>
        sendUnexpectedFailureDiagnostics(uri, Seq(vf))
      case hc: HasCheckErrors =>
        val diagnostics = hc.errors.flatMap { chk: CheckError =>
          val mainDiagOpt = chk.originsWithMessages(_.o).headOption.flatMap {
            case (origin, msg) => originToDiagnostic(origin, msg)
          }
          val related = chk.originsWithMessages(_.o).drop(1)
            .flatMap { case (origin, msg) =>
              originToDiagnostic(origin, msg).map { diag =>
                new DiagnosticRelatedInformation(
                  new Location(uri, diag.getRange),
                  diag.getMessage.getLeft,
                )
              }
            }

          mainDiagOpt match {
            case Some(mainDiag) =>
              if (related.nonEmpty) {
                mainDiag.setRelatedInformation(related.asJava)
              }
              Some(mainDiag)
            case None =>
              showError("MultiOriginFailure had no usable origin")
              None
          }
        }
        MyLanguageServer.client.publishDiagnostics(
          new PublishDiagnosticsParams(uri, diagnostics.asJava)
        )

      case otherErr =>
        findOrigin(otherErr) match {
          case Some(origin) =>
            val range = originToRange(origin)
            val diag = createVerificationErrorDiagnostic(otherErr, range)
            MyLanguageServer.client.publishDiagnostics(
              new PublishDiagnosticsParams(uri, List(diag).asJava)
            )
          case None =>
            showError(
              s"Verification failed, verification error without position, error type: ${otherErr.getClass.getSimpleName}"
            )
            publishNoPositionError(uri, otherErr)
        }
    }

  private def publishNoPositionError(
      uri: String,
      err: VerificationError,
  ): Unit = {
    val diagnostic = new Diagnostic()
    diagnostic.setSeverity(DiagnosticSeverity.Error)
    diagnostic.setMessage(err.getMessage)
    diagnostic.setRange(new Range(new Position(0, 0), new Position(0, 0)))
    diagnostic.setSource("VerCors")

    MyLanguageServer.client.publishDiagnostics(
      new PublishDiagnosticsParams(uri, List(diagnostic).asJava)
    )
  }

  private def originToRange(origin: Origin) = {
    origin.find[PositionRange].map {
      case PositionRange(startLine, endLine, Some((startCol, endCol))) =>
        new Range(
          new Position(startLine, startCol),
          new Position(endLine, endCol),
        )
      case PositionRange(startLine, endLine, None) =>
        new Range(new Position(startLine, 0), new Position(endLine, 0))
    }.getOrElse(new Range(new Position(0, 0), new Position(0, 0)))
  }

  private def createVerificationErrorDiagnostic(
      err: VerificationError,
      range: Range,
  ) = {
    val diagnostic = new Diagnostic()
    diagnostic.setSeverity(DiagnosticSeverity.Error)
    diagnostic.setRange(range)
    diagnostic.setSource("VerCors")
    diagnostic.setMessage(err.getMessage)
    diagnostic
  }

  private def findOrigin(obj: Any): Option[Origin] = {
    def tryGet(methodName: String): Option[Origin] = {
      obj.getClass.getMethods.find(m =>
        m.getName == methodName && m.getParameterCount == 0 &&
          classOf[Origin].isAssignableFrom(m.getReturnType)
      ).flatMap { method =>
        try Some(method.invoke(obj).asInstanceOf[Origin])
        catch { case _: Throwable => None }
      }
    }

    tryGet("o").orElse(tryGet("origin")).orElse {
      obj.getClass.getDeclaredFields
        .find(f => classOf[Origin].isAssignableFrom(f.getType))
        .flatMap { field =>
          field.setAccessible(true)
          try Some(field.get(obj).asInstanceOf[Origin])
          catch { case _: Throwable => None }
        }
    }.orElse {
      obj.getClass.getDeclaredFields
        .find(f => classOf[Node[_]].isAssignableFrom(f.getType))
        .flatMap { field =>
          field.setAccessible(true)
          try {
            val node = field.get(obj).asInstanceOf[Node[_]]
            Some(node.o)
          } catch { case _: Throwable => None }
        }
    }
  }
}
