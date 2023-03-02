package hre.util

import hre.config.Configuration

import java.io.{ByteArrayInputStream, File}
import sys.process._

import scala.jdk.CollectionConverters._
import java.nio.file.{Files, Paths}
import java.util.regex.Pattern

object Notifier {
  def notify(title: String, message: String): Boolean = Configuration.getOS() match {
    case Configuration.OS.WINDOWS => notifyWindows10(title, message)
    case Configuration.OS.MAC => notifyMacOS(title, message)
    case Configuration.OS.UNIX => notifyLibnotify(title, message)
    case _ => false
  }

  def notifyLibnotify(title: String, message: String): Boolean = {
    if (commandExists("notify-send")) {
      val cmd = Seq("notify-send", title, message)
      cmd ! ProcessLogger(_ => (), _ => ()) match {
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
      cmd ! ProcessLogger(_ => (), _ => ()) match {
        case 0 => true
        case _ => false
      }
    } else {
      false
    }
  }

  val powershellNotificationScript: String =
    """
      |$app = '{1AC14E77-02E7-4E5D-B744-2EB1AE5198B7}\WindowsPowerShell\v1.0\powershell.exe'
      |[Windows.UI.Notifications.ToastNotificationManager, Windows.UI.Notifications, ContentType = WindowsRuntime]
      |
      |$Template = [Windows.UI.Notifications.ToastTemplateType]::ToastImageAndText01
      |
      |#Gets the Template XML so we can manipulate the values
      |[xml]$ToastTemplate = ([Windows.UI.Notifications.ToastNotificationManager]::GetTemplateContent($Template).GetXml())
      |
      |[xml]$ToastTemplate = @"
      |<toast launch="app-defined-string">
      |  <visual>
      |    <binding template="ToastGeneric">
      |      <text>%s</text>
      |      <text>%s</text>
      |    </binding>
      |  </visual>
      |</toast>
      |"@
      |
      |$ToastXml = New-Object -TypeName Windows.Data.Xml.Dom.XmlDocument
      |$ToastXml.LoadXml($ToastTemplate.OuterXml)
      |
      |$notify = [Windows.UI.Notifications.ToastNotificationManager]::CreateToastNotifier($app)
      |
      |$notify.Show($ToastXml)
      |""".stripMargin

  def notifyWindows10(title: String, message: String): Boolean = {
    // Only allow safe characters
    val ok = (x: String) => Pattern.matches("[a-zA-Z ]*", x)
    if (!ok(title) || !ok(message)) {
      return false
    }

    // Script comes from: https://gist.github.com/Windos/9aa6a684ac583e0d38a8fa68196bc2dc
    val script = powershellNotificationScript.format(title, message)
    val is = new ByteArrayInputStream(script.getBytes("UTF-8"))
    val cmd = Seq("powershell", "-Command", "-")
    val p2 = cmd #< is
    p2.!(ProcessLogger(_ => (), _ => ())) == 0
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
