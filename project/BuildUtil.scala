object BuildUtil {
  def commitFromGitUrl(url: String): Option[String] = {
    if (!url.contains("#")) {
      None
    } else {
      Some(url.split('#')(1))
    }
  }
}
