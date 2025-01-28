package util

import mill._
import scalalib.{JavaModule => BaseJavaModule}

trait JavaModule extends BaseJavaModule {
  // https://github.com/viperproject/silicon/issues/748
  // 32MB is enough stack space for silicon, a 100% marco guarantee
  // Running VerCors with less than one gigabyte of free memory before
  // needing to GC is probably not very worthwhile. The maximum heap size
  // is left default (typically something like 1/4 of your physical memory)
  // so that it can be increased by the user.
  override def forkArgs = Seq("-Xms1G", "-Xss256m")

  def classPathFileElements = T { runClasspath().map(_.path.toString) }

  def unixClassPathArgumentFile =
    T {
      val cpString = classPathFileElements().mkString("\"", ":", "\"")
      val cpArg = "-cp " + cpString
      os.write(T.dest / "classpath", cpArg)
      T.dest / "classpath"
    }

  def strictOptionsFile = T.source { settings.root / ".compile-strict" }

  def strictOptions: T[Boolean] = T { os.exists(strictOptionsFile().path) }

  override def javacOptions =
    T {
      val shared = Seq("--release", "17", "-deprecation")

      if (strictOptions()) { Seq("-Werror") ++ shared }
      else {
        Seq(
          // nothing here yet
        ) ++ shared
      }
    }

  def windowsClassPathArgumentFile =
    T {
      val cpString = classPathFileElements().mkString("\"", ";", "\"")
      val cpArg = "-cp " + cpString
      os.write(T.dest / "classpath", cpArg)
      T.dest / "classpath"
    }

  def runScriptClasses = T { Map("run" -> finalMainClass()) }

  def runScript =
    T {
      // PB: this is nearly just Jvm.createLauncher, but you cannot set the filename, and uses a literal classpath instead of a file.
      for ((name, mainClass) <- runScriptClasses()) {
        // thanks https://gist.github.com/lhns/ee821a5cd1b2031856b21a0e78e1ecc9
        val quote = "\""
        val header = "@ 2>/dev/null # 2>nul & echo off & goto BOF"
        val unix = Seq(
          ":",
          s"java ${forkArgs().mkString(" ")} $quote@${unixClassPathArgumentFile()}$quote $mainClass $quote$$@$quote",
          "exit",
        )
        val batch = Seq(
          ":BOF",
          s"java ${forkArgs().mkString(" ")} $quote@${windowsClassPathArgumentFile()}$quote $mainClass %*",
          "exit /B %errorlevel%",
        )
        val script =
          header + "\r\n" + unix.mkString("\n") + "\n\r\n" +
            batch.mkString("\r\n") + "\r\n"
        val isWin = scala.util.Properties.isWin
        val wantBatch =
          isWin && !org.jline.utils.OSUtils.IS_CYGWIN &&
            !org.jline.utils.OSUtils.IS_MSYSTEM
        val fileName =
          if (wantBatch)
            name + ".bat"
          else
            name
        os.write(T.dest / fileName, script)
        if (!isWin)
          os.perms.set(T.dest / name, os.PermSet.fromString("rwxrwxr-x"))
      }
      T.dest
    }
}
