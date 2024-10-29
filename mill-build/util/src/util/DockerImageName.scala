package util

sealed trait DockerImageName {
  def host: Option[String]
  def path: Seq[String]

  override def toString: String =
    (host match {
      case None | Some("docker.io") => ""
      case Some(host) => s"$host/"
    }) +
      path.mkString("/")

  def withTag(tag: String = "latest"): String =
    s"$toString:$tag"
}

object DockerImageName {
  implicit val rw: upickle.default.ReadWriter[DockerImageName] = upickle.default.macroRW

  case class Plain(repository: String) extends DockerImageName {
    override def host: Option[String] = None
    override def path: Seq[String] = Seq(repository)
  }

  object Plain {
    implicit val rw: upickle.default.ReadWriter[Plain] = upickle.default.macroRW
  }

  case class Public(namespace: String, repository: String) extends DockerImageName {
    override def host: Option[String] = None
    override def path: Seq[String] = Seq(namespace, repository)
  }

  object Public {
    implicit val rw: upickle.default.ReadWriter[Public] = upickle.default.macroRW
  }
}