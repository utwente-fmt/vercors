/*
This is a separate file, because targets are generally invalidated if the build definition changes. The fetching target
being in a different file reduces that chance.
*/

import mill._

object z3 extends Module {
  def url = T { "https://www.sosy-lab.org/ivy/org.sosy_lab/javasmt-solver-z3/com.microsoft.z3-4.8.7.jar" }

  def classPath = T {
    os.write(T.dest / "z3.jar", requests.get.stream(url()))
    PathRef(T.dest / "z3.jar")
  }
}

object antlr extends Module {
  def url = T {
    "https://github.com/niomaster/antlr4/releases/download/4.8-extractors-2/antlr4.jar"
  }

  def classPath = T {
    os.write(T.dest / "antlr.jar", requests.get.stream(url()))
    PathRef(T.dest / "antlr.jar")
  }
}