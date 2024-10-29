package vct.col.origin

sealed trait Name {
  def snake: String
  def usnake: String
  def camel: String
  def ucamel: String

  override def toString: String = throw new UnsupportedOperationException()
}

object Name {
  case class Required(name: String) extends Name {
    override def snake: String = name
    override def usnake: String = name
    override def camel: String = name
    override def ucamel: String = name
  }

  case class Preferred(parts: Seq[String]) extends Name {
    assert(parts.nonEmpty)
    override def snake: String = parts.map(_.toLowerCase).mkString("_")
    override def usnake: String = parts.map(_.toUpperCase).mkString("_")
    override def camel: String =
      (parts.head.toLowerCase +: parts.tail.map(_.toLowerCase.capitalize))
        .mkString("")
    override def ucamel: String = camel.capitalize
  }

  case class Join(left: Name, right: Name) extends Name {
    override def snake: String = s"${left.snake}_${right.snake}"
    override def usnake: String = s"${left.usnake}_${right.usnake}"
    override def camel: String = s"${left.camel}${right.ucamel}"
    override def ucamel: String = camel.capitalize
  }

  def apply(name: String): Name = Preferred(Seq(name))
  def strings(parts: String*): Name = Preferred(parts)
  def names(parts: Name*): Name = {
    assert(parts.nonEmpty)
    parts.reduce(Join)
  }
}
