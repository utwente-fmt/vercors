package vct.main

import hre.config.ConfigurationNonStatic
import hre.lang.SystemNonStatic
import vct.main.options.CommandLineOptionsParser
import vct.main.passes.PassesGenerator

import java.io.File

object Main {
  def main(args: Array[String]): Unit = {
    val classpath = System.getProperty("java.class.path")
    val classpathEntries = classpath.split(File.pathSeparator)
    System.out.println("Start classpath")
    for (entry <- classpathEntries) {
      System.out.println(entry)
    }
    System.out.println("end classpath")

    var loggingSetup = new LoggingSetup
    var passesExecutioner = new PassesExecutioner
    var passesGenerator = new PassesGenerator
    var fileParser = new FileParser
    var optionsParser = new CommandLineOptionsParser
    var system = new SystemNonStatic
    var configuration = new ConfigurationNonStatic
    var program = new Program(loggingSetup,passesExecutioner,passesGenerator,fileParser,optionsParser,system,configuration)
    System.exit(program.run(args))
  }
}
