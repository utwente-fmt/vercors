package hre.lsp.main

import hre.lsp.channel.{LspCarrierChannel, TracingJsonObjectChannel}
import hre.lsp.runner.Runner

import java.io.{BufferedInputStream, BufferedOutputStream, PrintStream}
import java.nio.file.{Files, Paths}

object Stdio {
  def main(args: Array[String]): Unit = {
    val channel = LspCarrierChannel(
      new BufferedInputStream(System.in),
      new BufferedOutputStream(System.out),
    )

    val traceWrite =
      new BufferedOutputStream(Files.newOutputStream(
        Paths.get("/home/pieter/vercors/tmp/trace-write.log")
      ))

    val traceRead =
      new BufferedOutputStream(Files.newOutputStream(
        Paths.get("/home/pieter/vercors/tmp/trace-read.log")
      ))

    val debugChannel = TracingJsonObjectChannel(channel, traceRead, traceWrite)

    System.setOut(System.err)

    Runner(debugChannel, null).run()
  }
}
