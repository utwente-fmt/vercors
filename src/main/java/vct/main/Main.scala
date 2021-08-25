package vct.main

import hre.config.ConfigurationNonStatic
import hre.lang.SystemNonStatic
import vct.main.options.CommandLineOptionsParser
import vct.main.passes.PassesGenerator

object Main {
  var loggingSetup = new LoggingSetup
  var passesExecutioner = new PassesExecutioner
  var passesGenerator = new PassesGenerator
  var fileParser = new FileParser
  var optionsParser = new CommandLineOptionsParser
  var system = new SystemNonStatic
  var configuration = new ConfigurationNonStatic
  var program = new Program(loggingSetup,passesExecutioner,passesGenerator,fileParser,optionsParser,system,configuration)
  def main(args: Array[String]): Unit = System.exit(program.run(args))
}
