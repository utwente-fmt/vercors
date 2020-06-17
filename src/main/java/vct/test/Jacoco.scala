package vct.test

import java.nio.file.Paths

import hre.config.Configuration
import hre.lang.System.{Debug, Output, Abort}

object Jacoco {
  def aggregateExecFiles(execPaths: Seq[String], outputExecPath: String): Unit = {
    val jacocoCli = Configuration.getJacocoCli
    jacocoCli.addArg("merge")

    execPaths.foreach((execPathStr) => {
      val execPath = Paths.get(execPathStr);

      if (!execPath.toFile.exists()) {
        Abort("File was passed in that does not exist: %s", execPath)
      }

      if (execPath.endsWith(".exec")) {
        jacocoCli.addArg(execPath.toAbsolutePath.toString)
        Debug("Adding: %s", execPath.toAbsolutePath().toString)
      } else {
        Abort("File was passed in that is not a .exec file: %s", execPath)
      }
    })

    Output("Aggegrating coverages...")
    val task = Task(jacocoCli, Seq())
    task.call
    Debug("Jacoco tool output")
    for (msg <- task.log) {
      Debug(msg.getFormat, msg.getArgs:_*)
    }

    Unit
  }
}
