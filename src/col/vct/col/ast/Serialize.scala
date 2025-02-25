package vct.col.ast

object Serialize {
  def serialize[G](x: Program[G]): vct.col.ast.serialize.Program = {
    val declarations =
      x.collect { case decl: Declaration[G] => decl }.zipWithIndex.map {
        case (decl, id) => decl -> id.toLong
      }.toMap

    x.serializeFamily(declarations)
  }
}
