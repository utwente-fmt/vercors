package vct.col.origin

sealed trait Name {
  def snake: String
  def camel: String
  def ucamel: String
}

object Name {
  case class Required(name: String) extends Name {
    override def snake: String = name
    override def camel: String = name
    override def ucamel: String = name
  }

  case class Preferred(parts: Seq[String]) extends Name {
    override def snake: String = parts.map(_.toLowerCase).mkString("_")
    override def camel: String = (parts.head.toLowerCase +: parts.map(_.toLowerCase.capitalize)).mkString("")
    override def ucamel: String = camel.capitalize
  }
}