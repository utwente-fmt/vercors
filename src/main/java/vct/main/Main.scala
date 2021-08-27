package vct.main

import hre.config.ConfigurationNonStatic
import hre.lang.SystemNonStatic
import vct.main.options.CommandLineOptionsParser
import vct.main.passes.PassesGenerator

import java.io.File

object Main {
  def main(args: Array[String]): Unit = {
    val loggingSetup = new LoggingSetup
    val passesExecutioner = new PassesExecutioner
    val passesGenerator = new PassesGenerator
    val fileParser = new FileParser
    val optionsParser = new CommandLineOptionsParser
    val system = new SystemNonStatic
    val configuration = new ConfigurationNonStatic
    val program = new Program(loggingSetup,passesExecutioner,passesGenerator,fileParser,optionsParser,system,configuration)
    System.exit(program.run(args))
  }
}
