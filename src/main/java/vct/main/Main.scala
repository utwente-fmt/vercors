package vct.main

import vct.main.options.OptionsParser
import vct.main.passes.PassesGenerator

object Main {
  var loggingSetup = new LoggingSetup
  var passesExecutioner = new PassesExecutioner
  var passesGenerator = new PassesGenerator
  var fileParser = new FileParser
  var optionsParser = new OptionsParser
  var program = new Program(loggingSetup,passesExecutioner,passesGenerator,fileParser,optionsParser)

  def main(args: Array[String]): Unit = System.exit(program.run(args))
}
