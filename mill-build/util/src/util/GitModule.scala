package util

import mill.{T, Module, pathReadWrite}

trait GitModule extends Module {
  def url: T[String]

  def commitish: T[String]

  def fetchSubmodulesRecursively: T[Boolean] = false

  def repo = T {
    os.proc("git", "init", "-q").call(cwd = T.dest)
    os.proc("git", "remote", "add", "origin", url()).call(cwd = T.dest)
    os.proc("git", "fetch", "--depth", "1", "origin", commitish()).call(cwd = T.dest)
    os.proc("git", "config", "advice.detachedHead", "false").call(cwd = T.dest)
    os.proc("git", "checkout", "FETCH_HEAD").call(cwd = T.dest)
    if(fetchSubmodulesRecursively())
      os.proc("git", "submodule", "update", "--init", "--recursive").call(cwd = T.dest)
    os.walk(T.dest).foreach(_.toIO.setWritable(true))
    os.remove.all(T.dest / ".git")
    T.dest
  }
}