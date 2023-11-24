package vct.col.origin

sealed trait Name {
  def snake: String
  def usnake: String
  def camel: String
  def ucamel: String
}

object Name {
  case class Required(name: String) extends Name {
    override def snake: String = name
    override def usnake: String = name
    override def camel: String = name
    override def ucamel: String = name
  }

  case class Preferred(parts: Seq[String]) extends Name {
    override def snake: String = parts.map(_.toLowerCase).mkString("_")
    override def usnake: String = parts.map(_.toUpperCase).mkString("_")
    override def camel: String = (parts.head.toLowerCase +: parts.tail.map(_.toLowerCase.capitalize)).mkString("")
    override def ucamel: String = camel.capitalize
  }
}