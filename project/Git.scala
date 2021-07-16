import java.io.File
import java.nio.file.{Files, Paths}
import scala.sys.process._

object Git {
  def gitHasChanges =
    if (hasGit) {
      if ((Seq("git", "diff-index", "--quiet", "HEAD", "--") ! ProcessLogger(a => (), b => ())) == 1) {
        "with changes"
      } else {
        "no changes"
      }
    } else {
      "unknown if there are changes"
    }

  def currentBranch: String =
    if (hasGit) {
      (Seq("git", "rev-parse", "--abbrev-ref", "HEAD") !!).stripLineEnd
    } else {
      "unknown branch"
    }

  def hasGit = commandExists("git") || commandExists("git.exe")

  // This method is probably duplicated because it's unclear how to share code between build.sbt and regular project code
  def commandExists(cmd: String): Boolean = System.getenv("PATH")
    .split(File.pathSeparator)
    .exists(path => {
      val p = Paths.get(path).resolve(cmd)
      Files.exists(p) && !Files.isDirectory(p) && Files.isExecutable(p)
    })

  def currentShortCommit: String =
    if (hasGit) {
      (Seq("git", "rev-parse", "--short", "HEAD") !!).stripLineEnd
    } else {
      "unknown commit"
    }
}
