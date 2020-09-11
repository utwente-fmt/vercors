package hre.util

import sys.process._
import hre.lang.System.Warning

object Notifier {
  def notify(title: String, message: String): Boolean = {
    val os = System.getProperty("os.name").toLowerCase()
    if (os.contains("win")) {
      notifyWindows10(title, message)
    } else if (os.contains("mac")) {
      notifyMacOS(title, message)
    } else if (os.contains("nix") || os.contains("linux")) {
      notifyUbuntu(title, message)
    } else {
      false
    }
  }

  def notifyUbuntu(title: String, message: String): Boolean = {
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
    // https://howchoo.com/mac/display-macos-notifications-command-line-scripts
    Warning("Notifying on mac not yet supported")
    false
  }

  def notifyWindows10(title: String, message: String): Boolean = {
    // https://gist.github.com/altrive/72594b8427b2fff16431
    Warning("Notifying on windows 10 not yet supported")
    false
  }

  def commandExists(cmd: String): Boolean = {
    true
  }
}
