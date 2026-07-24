package vct.main.modes

import com.typesafe.scalalogging.LazyLogging
import hre.progress.task.RootTask
import lsp.MyLanguageServer
import org.eclipse.lsp4j.jsonrpc.MessageConsumer
import org.eclipse.lsp4j.launch.LSPLauncher
import vct.main.Main
import vct.options.Options

import java.util.concurrent.{ExecutorService, Executors}
import java.util.function.Function

object LSP extends LazyLogging {
  def runOptions(options: Options): Int = {

    logger.info("Starting LSP server...")

    val server = new MyLanguageServer()
    val executorService: ExecutorService = Executors.newSingleThreadExecutor()

    var future = executorService.submit(new Runnable {
      override def run(): Unit = { RootTask().start() }
    })
    future.get()

    val wrapper: Function[MessageConsumer, MessageConsumer] = Function
      .identity()

    val launcher = LSPLauncher.createServerLauncher(
      server,
      System.in,
      System.out,
      executorService,
      wrapper,
    )

    server.setClient(launcher.getRemoteProxy)
    future = launcher.startListening()
    future.get()

    Main.EXIT_CODE_SUCCESS
  }
}
