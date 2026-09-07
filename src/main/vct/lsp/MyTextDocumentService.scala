package vct.lsp

import com.typesafe.scalalogging.LazyLogging
import org.eclipse.lsp4j.jsonrpc.messages.{Either, Either3}
import org.eclipse.lsp4j.services.TextDocumentService
import org.eclipse.lsp4j._
import vct.col.ast.{
  Constructor,
  Deref,
  InstanceMethod,
  InvokingNode,
  Local,
  ModelDeref,
  Node,
  Procedure,
  Verification,
}
import upickle.default._
import vct.col.origin.{BlameCollector, Name, Origin, PositionRange}
import vct.col.rewrite.Generation
import vct.lsp.LspMessages._
import vct.lsp.VerificationErrorsUtils.sendVerificationErrorDiagnostic
import vct.main.stages.{Parsing, Resolution}
import vct.options.Options
import vct.options.types.PathOrStd
import vct.parsers.ParseResult
import vct.parsers.transform.ConstantBlameProvider
import vct.result.VerificationError

import java.net.URI
import java.nio.file.Paths
import java.util
import java.util.Collections
import java.util.concurrent.CompletableFuture
import scala.collection.concurrent.TrieMap
import scala.collection.immutable.TreeMap
import scala.collection.mutable
import scala.io.Source
import scala.jdk.CollectionConverters._

class MyTextDocumentService extends TextDocumentService with LazyLogging {
  private val docs = TrieMap.empty[String, String]
  private var parsingResults: Option[ParseResult[Nothing]] = None
  private var resolutionResults: Option[Verification[_ <: Generation]] = None
  var originMap: TreeMap[(Int, Int, Int), (Int, Int, Int)] = TreeMap.empty
  private val dirty = TrieMap.empty[String, Boolean]
  private lazy val javaCompletions: List[CompletionItem] = loadCompletions(
    "lsp/language-server-java-c-matches.json"
  )
  private lazy val pvlCompletions: List[CompletionItem] = loadCompletions(
    "lsp/language-server-pvl-matches.json"
  )

  override def didOpen(params: DidOpenTextDocumentParams): Unit = {
    val uri = params.getTextDocument.getUri
    docs(uri) = params.getTextDocument.getText
    dirty.put(uri, false)
    analyzeDocument(uri)
  }

  override def didSave(params: DidSaveTextDocumentParams): Unit = {
    val uri = params.getTextDocument.getUri
    dirty.put(uri, false)
    analyzeDocument(uri)
  }

  override def didChange(params: DidChangeTextDocumentParams): Unit = {
    val uri = params.getTextDocument.getUri
    val text = params.getContentChanges.get(0).getText
    docs(uri) = text
    dirty.put(uri, true)
    analyzeDocument(uri)
  }

  def isDirty(uri: String): Boolean = dirty.getOrElse(uri, false)

  override def didClose(params: DidCloseTextDocumentParams): Unit = {}

  override def completion(params: CompletionParams): CompletableFuture[
    Either[java.util.List[CompletionItem], CompletionList]
  ] = {
    val uri = params.getTextDocument.getUri
    val pos = params.getPosition
    val prefix = extractPrefix(docs.getOrElse(uri, ""), pos)

    val ext = uri.split("\\.").lastOption.getOrElse("")
    val pool =
      ext match {
        case "java" | "c" | "cpp" => javaCompletions
        case "pvl" => pvlCompletions
        case _ => javaCompletions ++ pvlCompletions
      }

    val jsonFiltered = pool.filter(_.getLabel.startsWith(prefix))

    val symbolCompletions: List[CompletionItem] = resolutionResults.toList
      .flatMap { res =>
        val uses = collectNameUses(res.tasks.head.program)
        uses.map { case (_, declOrigin) =>
          val rawName = declOrigin.getPreferredNameOrElse()
          val nameStr = formatName(rawName)
          val item = new CompletionItem(nameStr)
          item.setKind(CompletionItemKind.Variable)
          item
        }.groupBy(_.getLabel).values.map(_.head).toList
      }

    val allCompletions = (jsonFiltered ++ symbolCompletions)
      .filter(_.getLabel.startsWith(prefix))

    val list = new CompletionList(allCompletions.asJava)
    CompletableFuture.completedFuture(Either.forRight(list))
  }

  override def documentSymbol(params: DocumentSymbolParams) = null

  override def resolveCompletionItem(
      unresolved: CompletionItem
  ): CompletableFuture[CompletionItem] = null

  override def definition(params: DefinitionParams): CompletableFuture[
    Either[java.util.List[_ <: Location], java.util.List[_ <: LocationLink]]
  ] = {
    val uri = params.getTextDocument.getUri
    val pos = params.getPosition
    val line = pos.getLine
    val char = pos.getCharacter

    val result: Either[java.util.List[_ <: Location], java.util.List[
      _ <: LocationLink
    ]] =
      originMap.maxBefore((line, char, Int.MaxValue)) match {
        case Some(((refLine, refStart, refEnd), (declLine, declStart, declEnd)))
            if refLine == line && refStart <= char && char <= refEnd =>
          val originRange =
            new Range(
              new Position(refLine, refStart),
              new Position(refLine, refEnd),
            )
          val targetRange =
            new Range(
              new Position(declLine, declStart),
              new Position(declLine, declEnd),
            )

          val locationLink = new LocationLink()
          locationLink.setOriginSelectionRange(originRange)
          locationLink.setTargetUri(uri)
          locationLink.setTargetRange(targetRange)
          locationLink.setTargetSelectionRange(targetRange)

          Either.forRight(Collections.singletonList(locationLink))

        case _ =>
          showInfo(
            s"No definition found at line ${pos.getLine}, character ${pos.getCharacter}"
          )
          Either.forRight(Collections.emptyList())
      }
    CompletableFuture.completedFuture(result)
  }

  private case class ParsedError(
      message: String,
      startLine: Int,
      startCol: Int,
      endLine: Int,
      endCol: Int,
  )

  private def analyzeDocument(uri: String): Unit = {
    MyLanguageServer.client.publishDiagnostics(
      new PublishDiagnosticsParams(uri, Collections.emptyList())
    )
    val path = Paths.get(new URI(uri))
    val options = Options().copy(inputs = List(PathOrStd.Path(path)))

    try {
      val collector = BlameCollector()
      val blameProvider = ConstantBlameProvider(collector)

      parsingResults = Some(
        Parsing.ofOptions(options, blameProvider).run(options.inputs)
      )
      resolutionResults = Some(
        Resolution.ofOptions(options, blameProvider).run(parsingResults.get)
      )

      val resolvedProgram = resolutionResults.get.tasks.head.program
      val uses = collectNameUses(resolvedProgram)
      originMap = TreeMap.from(hashNameUseOrigins(uses))
    } catch {
      case err: VerificationError.UserError =>
        val diagnostics = createDiagnostics(err)
        if (diagnostics.nonEmpty) {
          MyLanguageServer.client.publishDiagnostics(
            new PublishDiagnosticsParams(uri, diagnostics.asJava)
          )
        } else { sendVerificationErrorDiagnostic(uri, err) }

      case err: VerificationError =>
        sendVerificationErrorDiagnostic(uri, err)
        showError("Verification error")

      case ex: Exception =>
        showError(s"Unexpected error: ${ex.getMessage}: ${ex.getStackTrace
            .mkString("Array(", ", ", ")")}")
    }
  }

  case class Entry(name: String, `match`: String)
  case object Entry {
    implicit val rw: ReadWriter[Entry] = macroRW
  }

  private def loadCompletions(path: String): List[CompletionItem] = {
    read[List[Entry]](Source.fromResource(path).mkString).map { e =>
      val item = new CompletionItem(e.`match`)
      // TODO: use name to set kind
      item.setKind(CompletionItemKind.Keyword)
      item
    }
  }

  private def collectNameUses[T](root: Node[T]): Seq[(Origin, Origin)] = {
    val buf = mutable.Buffer.empty[(Origin, Origin)]
    val stack = mutable.Stack[Node[T]](root)

    while (stack.nonEmpty) {
      val n = stack.pop()
      n match {
        case l: Local[T] => buf += (l.o -> l.ref.decl.o)
        case d: Deref[T] => buf += (d.o -> d.ref.decl.o)
        case m: ModelDeref[T] => buf += (m.o -> m.ref.decl.o)
        case inv: InvokingNode[T] =>
          inv.ref.decl match {
            case m: InstanceMethod[T] =>
              m.body.foreach(stmt => buf += (inv.o -> stmt.o))

            case p: Procedure[T] =>
              p.body.foreach(stmt => buf += (inv.o -> stmt.o))

            case c: Constructor[T] =>
              c.body.foreach(stmt => buf += (inv.o -> stmt.o))
            case otherDecl => buf += (inv.o -> otherDecl.o)
          }
        case other =>
          other.o.getPreferredName.foreach { _ => buf += (other.o -> other.o) }
      }
      stack.pushAll(n.subnodes)
    }
    buf.toSeq
  }

  private def hashNameUseOrigins(
      uses: Seq[(Origin, Origin)]
  ): Map[(Int, Int, Int), (Int, Int, Int)] = {
    uses.flatMap { case (useO, declO) =>
      for {
        PositionRange(uLine, _, Some((uStartCol, uEndCol))) <- useO
          .find[PositionRange]
        PositionRange(dLine, _, Some((dStartCol, dEndCol))) <- declO
          .find[PositionRange]
      } yield { (uLine, uStartCol, uEndCol) -> (dLine, dStartCol, dEndCol) }
    }.toMap
  }

  private def extractPrefix(text: String, pos: Position): String = {
    val lines = text.split("\r?\n", -1)
    if (pos.getLine >= lines.length)
      return ""
    val line = lines(pos.getLine)
    val idx = pos.getCharacter.min(line.length)
    line.substring(0, idx).reverse
      .takeWhile(ch => ch.isLetterOrDigit || ch == '_').reverse
  }

  private def createDiagnostics(
      err: VerificationError.UserError
  ): List[Diagnostic] = {
    extractFirstParsedError(err).toList.map { pe =>
      val diagnostic = new Diagnostic()
      diagnostic.setSeverity(DiagnosticSeverity.Error)
      diagnostic.setMessage(pe.message)
      diagnostic.setRange(new Range(
        new Position(pe.startLine, pe.startCol),
        new Position(pe.endLine, pe.endCol),
      ))
      diagnostic
    }
  }

  private def extractFirstParsedError(
      err: VerificationError.UserError
  ): Option[ParsedError] =
    err match {
      case pe: vct.parsers.err.ParseError => extractFromParseError(pe)

      case vct.parsers.err.ParseErrors(errors) =>
        errors.iterator.map(extractFromParseError).collectFirst {
          case Some(pe) => pe
        }

      case _ => None
    }

  private def extractFromParseError(
      pe: vct.parsers.err.ParseError
  ): Option[ParsedError] = {
    pe.origin.find[PositionRange].flatMap { pos =>
      pos.startEndColIdx.map { case (startCol, endCol) =>
        ParsedError(
          message = pe.message,
          startLine = pos.startLineIdx,
          startCol = startCol,
          endLine = pos.endLineIdx,
          endCol = endCol,
        )
      }
    }
  }

  private def formatName(name: Name): String =
    name match {
      case Name.Preferred(parts) => parts.mkString
      case Name.Required(n) => n
      case Name.Join(a, b) => a.toString + b.toString
    }

  // These seemingly unnecessary overrides are necessary due to a scala bug
  override def hover(params: HoverParams): CompletableFuture[Hover] =
    super.hover(params)

  override def signatureHelp(
      params: SignatureHelpParams
  ): CompletableFuture[SignatureHelp] = super.signatureHelp(params)

  override def declaration(params: DeclarationParams): CompletableFuture[
    Either[util.List[_ <: Location], util.List[_ <: LocationLink]]
  ] = super.declaration(params)

  override def typeDefinition(params: TypeDefinitionParams): CompletableFuture[
    Either[util.List[_ <: Location], util.List[_ <: LocationLink]]
  ] = super.typeDefinition(params)

  override def implementation(params: ImplementationParams): CompletableFuture[
    Either[util.List[_ <: Location], util.List[_ <: LocationLink]]
  ] = super.implementation(params)

  override def references(
      params: ReferenceParams
  ): CompletableFuture[util.List[_ <: Location]] = super.references(params)

  override def documentHighlight(
      params: DocumentHighlightParams
  ): CompletableFuture[util.List[_ <: DocumentHighlight]] =
    super.documentHighlight(params)

  override def codeAction(
      params: CodeActionParams
  ): CompletableFuture[util.List[Either[Command, CodeAction]]] =
    super.codeAction(params)

  override def resolveCodeAction(
      unresolved: CodeAction
  ): CompletableFuture[CodeAction] = super.resolveCodeAction(unresolved)

  override def codeLens(
      params: CodeLensParams
  ): CompletableFuture[util.List[_ <: CodeLens]] = super.codeLens(params)

  override def resolveCodeLens(
      unresolved: CodeLens
  ): CompletableFuture[CodeLens] = super.resolveCodeLens(unresolved)

  override def formatting(
      params: DocumentFormattingParams
  ): CompletableFuture[util.List[_ <: TextEdit]] = super.formatting(params)

  override def rangeFormatting(
      params: DocumentRangeFormattingParams
  ): CompletableFuture[util.List[_ <: TextEdit]] = super.rangeFormatting(params)

  override def rangesFormatting(
      params: DocumentRangesFormattingParams
  ): CompletableFuture[util.List[_ <: TextEdit]] =
    super.rangesFormatting(params)

  override def onTypeFormatting(
      params: DocumentOnTypeFormattingParams
  ): CompletableFuture[util.List[_ <: TextEdit]] =
    super.onTypeFormatting(params)

  override def rename(params: RenameParams): CompletableFuture[WorkspaceEdit] =
    super.rename(params)

  override def linkedEditingRange(
      params: LinkedEditingRangeParams
  ): CompletableFuture[LinkedEditingRanges] = super.linkedEditingRange(params)

  override def willSave(params: WillSaveTextDocumentParams): Unit =
    super.willSave(params)

  override def willSaveWaitUntil(
      params: WillSaveTextDocumentParams
  ): CompletableFuture[util.List[TextEdit]] = super.willSaveWaitUntil(params)

  override def documentLink(
      params: DocumentLinkParams
  ): CompletableFuture[util.List[DocumentLink]] = super.documentLink(params)

  override def documentLinkResolve(
      params: DocumentLink
  ): CompletableFuture[DocumentLink] = super.documentLinkResolve(params)

  override def documentColor(
      params: DocumentColorParams
  ): CompletableFuture[util.List[ColorInformation]] =
    super.documentColor(params)

  override def colorPresentation(
      params: ColorPresentationParams
  ): CompletableFuture[util.List[ColorPresentation]] =
    super.colorPresentation(params)

  override def foldingRange(
      params: FoldingRangeRequestParams
  ): CompletableFuture[util.List[FoldingRange]] = super.foldingRange(params)

  override def prepareRename(params: PrepareRenameParams): CompletableFuture[
    Either3[Range, PrepareRenameResult, PrepareRenameDefaultBehavior]
  ] = super.prepareRename(params)

  override def prepareTypeHierarchy(
      params: TypeHierarchyPrepareParams
  ): CompletableFuture[util.List[TypeHierarchyItem]] =
    super.prepareTypeHierarchy(params)

  override def typeHierarchySupertypes(
      params: TypeHierarchySupertypesParams
  ): CompletableFuture[util.List[TypeHierarchyItem]] =
    super.typeHierarchySupertypes(params)

  override def typeHierarchySubtypes(
      params: TypeHierarchySubtypesParams
  ): CompletableFuture[util.List[TypeHierarchyItem]] =
    super.typeHierarchySubtypes(params)

  override def prepareCallHierarchy(
      params: CallHierarchyPrepareParams
  ): CompletableFuture[util.List[CallHierarchyItem]] =
    super.prepareCallHierarchy(params)

  override def callHierarchyIncomingCalls(
      params: CallHierarchyIncomingCallsParams
  ): CompletableFuture[util.List[CallHierarchyIncomingCall]] =
    super.callHierarchyIncomingCalls(params)

  override def callHierarchyOutgoingCalls(
      params: CallHierarchyOutgoingCallsParams
  ): CompletableFuture[util.List[CallHierarchyOutgoingCall]] =
    super.callHierarchyOutgoingCalls(params)

  override def selectionRange(
      params: SelectionRangeParams
  ): CompletableFuture[util.List[SelectionRange]] = super.selectionRange(params)

  override def semanticTokensFull(
      params: SemanticTokensParams
  ): CompletableFuture[SemanticTokens] = super.semanticTokensFull(params)

  override def semanticTokensFullDelta(
      params: SemanticTokensDeltaParams
  ): CompletableFuture[Either[SemanticTokens, SemanticTokensDelta]] =
    super.semanticTokensFullDelta(params)

  override def semanticTokensRange(
      params: SemanticTokensRangeParams
  ): CompletableFuture[SemanticTokens] = super.semanticTokensRange(params)

  override def moniker(
      params: MonikerParams
  ): CompletableFuture[util.List[Moniker]] = super.moniker(params)

  override def inlayHint(
      params: InlayHintParams
  ): CompletableFuture[util.List[InlayHint]] = super.inlayHint(params)

  override def resolveInlayHint(
      unresolved: InlayHint
  ): CompletableFuture[InlayHint] = super.resolveInlayHint(unresolved)

  override def inlineValue(
      params: InlineValueParams
  ): CompletableFuture[util.List[InlineValue]] = super.inlineValue(params)

  override def diagnostic(
      params: DocumentDiagnosticParams
  ): CompletableFuture[DocumentDiagnosticReport] = super.diagnostic(params)

  override def inlineCompletion(
      params: InlineCompletionParams
  ): CompletableFuture[
    Either[util.List[InlineCompletionItem], InlineCompletionList]
  ] = super.inlineCompletion(params)
}
