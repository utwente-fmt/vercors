package gen
import upickle.default.{ReadWriter => RW, macroRW}

/**
 * Common Grammar Language
 */
case class CGL(scopeName: String, fileTypes: String, patterns: Seq[MatchPattern]) {
  def addPattern(mp: MatchPattern): CGL = {
    CGL(scopeName, fileTypes, patterns = patterns :+ mp)
  }
}
object CGL{
  implicit val rw: RW[CGL] = macroRW
}



case class MatchPattern(name: String, `match`: String)
object MatchPattern {
  implicit val rw: RW[MatchPattern] = macroRW
}