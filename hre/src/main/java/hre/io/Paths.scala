package hre.io

import java.io.File
import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters._

object Paths {
  def commandExists(cmd: String): Boolean = {
    System.getenv().asScala.getOrElse("PATH", "")
      .split(File.pathSeparator)
      .exists(path => {
        val p = java.nio.file.Paths.get(path).resolve(cmd)
        Files.exists(p) && !Files.isDirectory(p) && Files.isExecutable(p)
      })
  }
}
