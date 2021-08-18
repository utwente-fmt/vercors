package vct.main

import hre.lang.SystemNonStatic
import vct.main.options.OptionsParser
import vct.main.passes.PassesGenerator

object Main {
  var loggingSetup = new LoggingSetup
  var passesExecutioner = new PassesExecutioner
  var passesGenerator = new PassesGenerator
  var fileParser = new FileParser
  var optionsParser = new OptionsParser
  var system = new SystemNonStatic
  var program = new Program(loggingSetup,passesExecutioner,passesGenerator,fileParser,optionsParser,system)

  def main(args: Array[String]): Unit = System.exit(program.run(args))
}
