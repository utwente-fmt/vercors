package vct.lsp

import com.google.gson.{JsonArray, JsonElement, JsonObject, JsonPrimitive}
import hre.progress.TaskRegistry
import hre.progress.task.{AbstractTask, RootTask}
import vct.lsp.MyLanguageServer.cancelledTokens
import org.eclipse.lsp4j._
import org.eclipse.lsp4j.jsonrpc.messages.Either
import org.eclipse.lsp4j.services.WorkspaceService
import vct.col.origin._
import vct.col.rewrite.bip.BIP
import vct.lsp.LspMessages._
import vct.lsp.VerificationErrorsUtils.{
  sendUnexpectedFailureDiagnostics,
  sendVerificationErrorDiagnostic,
}
import vct.main.stages._
import vct.options.Options
import vct.parsers.transform.ConstantBlameProvider
import vct.result.VerificationError
import viper.api.backend.silicon.{DataRecordTask, Util}

import java.net.URI
import java.nio.file.Paths
import java.util.concurrent.CompletableFuture
import java.util.concurrent.atomic.AtomicBoolean
import scala.jdk.CollectionConverters._
import scala.language.reflectiveCalls

class MyWorkspaceService extends WorkspaceService {
  private val keepMonitoring = new AtomicBoolean(true)

  override def didChangeConfiguration(
      params: DidChangeConfigurationParams
  ): Unit = {
    MyLanguageServer.client.logMessage(
      new MessageParams(MessageType.Info, "Workspace configuration changed")
    )
  }

  override def didChangeWatchedFiles(
      params: DidChangeWatchedFilesParams
  ): Unit = {
    MyLanguageServer.client
      .logMessage(new MessageParams(MessageType.Info, "Watched files changed"))
  }

  override def executeCommand(
      params: ExecuteCommandParams
  ): CompletableFuture[AnyRef] = {
    params.getCommand match {
      case "vercors.lspVerify" =>
        val uri =
          params.getArguments.get(0).asInstanceOf[JsonPrimitive].getAsString

        if (abortIfDirty(uri))
          return CompletableFuture.completedFuture(null)

        val path = Paths.get(new URI(uri))

        val syntheticArgs = collection.mutable.ListBuffer("--verify")
        if (params.getArguments.size > 1) {
          val arg1 = params.getArguments.get(1)
          arg1 match {
            case json: JsonElement =>
              if (json.isJsonObject) {
                val obj = json.getAsJsonObject

                def addArray(key: String): Unit =
                  if (obj.has(key)) {
                    obj.get(key) match {
                      case arr if arr.isJsonArray =>
                        arr.getAsJsonArray.iterator().asScala.foreach { elem =>
                          syntheticArgs += elem.getAsString
                        }
                      case str if str.isJsonPrimitive =>
                        str.getAsString.trim.split("\\s+")
                          .foreach(syntheticArgs += _)
                      case _ =>
                    }
                  }

                def addSingleFlagWithValue(flag: String, key: String): Unit =
                  if (obj.has(key)) {
                    val value = obj.get(key).getAsString
                    syntheticArgs += flag
                    syntheticArgs += value
                  }

                addArray("flags")
                addArray("customFlags")
                addArray("rawArgs")

                addSingleFlagWithValue("--backend", "backend")
              }
            case _ =>
          }
        }
        syntheticArgs += path.toString

        val options = vct.options.Options.parse(syntheticArgs.toArray)
          .getOrElse(
            throw new IllegalArgumentException("Failed to parse options")
          )

        // val options = Options().copy(inputs = List(PathOrStd.Path(path)))
        val rawToken = params.getWorkDoneToken
        val token =
          if (rawToken.isLeft)
            rawToken.getLeft
          else
            rawToken.getRight.toString
        val createParams = new WorkDoneProgressCreateParams()
        createParams.setToken(token)

        try { MyLanguageServer.client.createProgress(createParams) }
        catch {
          case ex: Exception =>
            showError(s"Error calling createProgress: ${ex.getMessage}")
        }

        val rootTask = TaskRegistry.getRootTask
        keepMonitoring.set(true)
        CompletableFuture
          .runAsync(() => runVerificationStages(options, token, uri, rootTask))
          .thenApply(_ => null)

      case unknown =>
        CompletableFuture.failedFuture(new IllegalArgumentException(
          s"Unknown command: $unknown"
        ))
    }
  }

  private def abortIfDirty(uri: String): Boolean = {
    if (MyLanguageServer.textService.isDirty(uri)) {
      showWarning(
        "You have unsaved changes in this file. Please save before verifying."
      )
      true
    } else { false }
  }

  private def runVerificationStages(
      options: Options,
      token: String,
      uri: String,
      rootTask: RootTask,
  ): Unit = {
    TaskRegistry().install()
    rootTask.start()
    startVerificationMonitor(TaskRegistry.getRootTask, uri)

    try {
      val collector = BlameCollector()
      val blameProvider = ConstantBlameProvider(collector)
      val bipResults = BIP.VerificationResults()

      val begin = new WorkDoneProgressBegin()
      begin.setTitle("Verifying")
      begin.setCancellable(true)
      begin.setPercentage(0)
      notifyProgress(token, begin)

      val totalStages = 5
      var currentStage = 0

      def report(stageName: String): Unit = {
        currentStage += 1
        val report = new WorkDoneProgressReport()
        report.setMessage(s"Running $stageName...")
        report.setPercentage(currentStage * 100 / totalStages)
        notifyProgress(token, report)
      }

      if (checkCancelled(token))
        return
      report("Parsing")
      val parsing = Parsing.ofOptions(options, blameProvider)
        .run(options.inputs)

      if (checkCancelled(token))
        return
      report("Resolution")
      val resolution = Resolution.ofOptions(options, blameProvider).run(parsing)

      if (checkCancelled(token))
        return
      report("Transformation")
      val transformation = Transformation.ofOptions(options, bipResults)
        .run(resolution)

      if (checkCancelled(token))
        return
      report("Backend")
      val backend = Backend.ofOptions(options).run(transformation)

      if (checkCancelled(token))
        return
      report("ExpectedErrors")
      ExpectedErrors.ofOptions(options).run(backend)

      keepMonitoring.set(false)
      val failures = collector.errs.toSeq
      sendUnexpectedFailureDiagnostics(uri, failures)

      val errorDescriptions = failures.map(_.desc).mkString("\n")
      MyLanguageServer.client.logMessage(new MessageParams(
        MessageType.Error,
        s"Actual verification failures:\n$errorDescriptions",
      ))

      if (failures.isEmpty) { showInfo("Verification completed successfully") }
      else { showError("Verification completed with failures") }
    } catch {
      case err: VerificationError =>
        val end = new WorkDoneProgressEnd()
        end.setMessage(s"Verification failed: ${err.getMessage}")
        notifyProgress(token, end)
        sendVerificationErrorDiagnostic(uri, err)
        showError("Verification failed with Error")

      case ex: Exception =>
        val end = new WorkDoneProgressEnd()
        end.setMessage("Verification aborted due to an unexpected error")
        notifyProgress(token, end)
        showError(
          "Unexpected error during verification. Check the VerCors log for details."
        )

        val full =
          s"Unexpected exception: ${ex.getMessage}\n" +
            ex.getStackTrace.mkString("", "\n", "")
        MyLanguageServer.client
          .logMessage(new MessageParams(MessageType.Error, full))
    }
  }

  private def checkCancelled(token: String): Boolean = {
    if (cancelledTokens.contains(token)) {
      cancelledTokens.remove(token)
      val end = new WorkDoneProgressEnd()
      end.setMessage(s"Verification cancelled by user")
      notifyProgress(token, end)
      true
    } else
      false
  }

  private def notifyProgress[T <: WorkDoneProgressNotification](
      token: String,
      value: T,
  ): Unit = {
    val params = new ProgressParams()
    params.setToken(token)
    params.setValue(Either.forLeft(value))
    MyLanguageServer.client.notifyProgress(params)
  }

  private def collectDataRecordTasks(
      root: AbstractTask
  ): Seq[DataRecordTask] = {
    val result = scala.collection.mutable.ArrayBuffer[DataRecordTask]()
    val stack = scala.collection.mutable.Stack[AbstractTask](root)

    while (stack.nonEmpty) {
      val current = stack.pop()

      current match {
        case task: DataRecordTask => result += task
        case _ =>
      }

      stack.pushAll(current.subTasks)
    }

    result.toSeq
  }

  private def startVerificationMonitor(
      root: AbstractTask,
      uri: String,
  ): Unit = {
    val monitor =
      new Thread(() => monitorLoop(root, uri), "VerCorsVerificationMonitor")
    monitor.setDaemon(true)
    monitor.start()
  }

  private def monitorLoop(root: AbstractTask, uri: String): Unit = {
    var lastSent = Set.empty[(Int, Int, Int, Int)]
    try {
      while (keepMonitoring.get()) {
        val dataTasks = collectDataRecordTasks(root)
        logCurrentOrigins(dataTasks)

        val verified = extractRanges(dataTasks)
        val newRanges = filterNewRanges(verified, lastSent)
        lastSent ++= rangeKeys(newRanges)

        if (newRanges.nonEmpty)
          sendVerifiedRanges(uri, newRanges)
        Thread.sleep(1000)
      }
    } catch { case ex: Exception => logWarning("Error while monitoring", ex) }
  }

  private def logCurrentOrigins(dataTasks: Seq[DataRecordTask]): Unit = {
    dataTasks.flatMap(task => Util.getOrigin(task.record.value)) match {
      case origins if origins.nonEmpty =>
        MyLanguageServer.client
          .logMessage(new MessageParams(MessageType.Info, s"Origin $origins"))
      case _ =>
    }
  }

  private def extractRanges(dataTasks: Seq[DataRecordTask]): Seq[Range] =
    dataTasks.flatMap { task =>
      Util.getOrigin(task.record.value).flatMap(_.find[PositionRange]).map {
        case PositionRange(sL, eL, Some((sC, eC))) =>
          new Range(new Position(sL, sC), new Position(eL, eC))
        case PositionRange(sL, eL, None) =>
          new Range(new Position(sL, 0), new Position(eL, 0))
      }
    }

  private def filterNewRanges(
      ranges: Seq[Range],
      lastSent: Set[(Int, Int, Int, Int)],
  ): Seq[Range] =
    ranges.filter { r =>
      val key =
        (
          r.getStart.getLine,
          r.getStart.getCharacter,
          r.getEnd.getLine,
          r.getEnd.getCharacter,
        )
      !lastSent.contains(key)
    }

  private def rangeKeys(ranges: Seq[Range]): Seq[(Int, Int, Int, Int)] =
    ranges.map(r =>
      (
        r.getStart.getLine,
        r.getStart.getCharacter,
        r.getEnd.getLine,
        r.getEnd.getCharacter,
      )
    )

  private def sendVerifiedRanges(uri: String, newRanges: Seq[Range]): Unit = {
    val params = new JsonObject()
    params.addProperty("uri", uri)
    val array = new JsonArray()
    newRanges.foreach { r =>
      val obj = new JsonObject()
      obj.add("start", makePos(r.getStart))
      obj.add("end", makePos(r.getEnd))
      array.add(obj)
    }
    params.add("ranges", array)
    invokeNotify("vercors/verifiedRange", params)
    MyLanguageServer.client.logMessage(new MessageParams(
      MessageType.Info,
      s"Sent $newRanges new verified ranges",
    ))
  }

  private def makePos(p: Position): JsonObject = {
    val o = new JsonObject()
    o.addProperty("line", p.getLine)
    o.addProperty("character", p.getCharacter)
    o
  }

  private def invokeNotify(method: String, params: JsonObject): Unit = {
    MyLanguageServer.client.getClass
      .getMethod("notify", classOf[String], classOf[Object])
      .invoke(MyLanguageServer.client, method, params)
  }

  private def logWarning(msg: String, ex: Throwable): Unit = {
    MyLanguageServer.client.logMessage(new MessageParams(
      MessageType.Warning,
      s"[Monitor] $msg: ${ex.getMessage}",
    ))
  }
}
