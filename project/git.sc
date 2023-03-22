import mill._

trait GitModule extends Module {
  def url: T[String]
  def commitish: T[String]

  def repo = T {
    os.proc("git", "init", "-b", "dontcare").call(cwd = T.dest)
    os.proc("git", "remote", "add", "origin", url()).call(cwd = T.dest)
    os.proc("git", "fetch", "--depth", "1", "origin", commitish()).call(cwd = T.dest)
    os.proc("git", "config", "advice.detachedHead", "false").call(cwd = T.dest)
    os.proc("git", "checkout", "FETCH_HEAD").call(cwd = T.dest)
    PathRef(T.dest)
  }
}