package vct.main.passes

object ChainPart {
  def inflate(parts: Seq[ChainPart]): Seq[Seq[String]] =
    parts.headOption match {
      case None => Seq(Seq())
      case Some(pass: Do) => inflate(parts.tail).map(pass.key +: _)
      case Some(Choose(alts@_* /* collect varargs into alts */)) =>
        val tail = inflate(parts.tail)
        alts.map(inflate).flatMap(branch => branch.flatMap(choice => tail.map(choice ++ _)))
    }
}
sealed trait ChainPart
case class Do(key: String) extends ChainPart
case class Choose(choices: Seq[ChainPart]*) extends ChainPart