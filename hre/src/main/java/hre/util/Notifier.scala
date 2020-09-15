package hre.util

import java.io.File

import sys.process._
import hre.lang.System.Warning

import scala.collection.JavaConverters._
import java.nio.file.{Files, Paths}

object Notifier {
  def notify(title: String, message: String): Boolean = {
    val os = System.getProperty("os.name").toLowerCase()
    if (os.contains("win")) {
      notifyWindows10(title, message)
    } else if (os.contains("mac")) {
      notifyMacOS(title, message)
    } else if (os.contains("nix") || os.contains("linux")) {
      notifyDebian(title, message)
    } else {
      false
    }
  }

  def notifyDebian(title: String, message: String): Boolean = {
    if (commandExists("notify-send")) {
      val cmd = Seq("notify-send", title, message)
      cmd ! ProcessLogger(_ => Unit, _ => Unit) match {
        case 0 => true
        case _ => false
      }
    } else {
      false
    }
  }

  def notifyMacOS(title: String, message: String): Boolean = {
    if (commandExists("osascript")) {
      val cmd = Seq("osascript", "-e", s"""display notification "$message" with title "$title"""")
      cmd ! ProcessLogger(_ => Unit, _ => Unit) match {
        case 0 => true
        case _ => false
      }
    } else {
      false
    }
  }

  def notifyWindows10(title: String, message: String): Boolean = {
    // https://gist.github.com/altrive/72594b8427b2fff16431
    // This one seems better: https://gist.github.com/Windos/9aa6a684ac583e0d38a8fa68196bc2dc
    Warning("Notifying on windows 10 not yet supported")
    false
  }

  def commandExists(cmd: String): Boolean = {
    System.getenv().asScala.getOrElse("PATH", "")
      .split(File.pathSeparator)
      .exists(path => {
        val p = Paths.get(path).resolve(cmd)
        Files.exists(p) && !Files.isDirectory(p) && Files.isExecutable(p)
      })
  }
}
