package vct.col.ast.family.javavar

import vct.col.ast.JavaVariableDeclaration
import vct.col.print._

trait JavaVariableDeclarationImpl[G] {
  this: JavaVariableDeclaration[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(
      Text(name) <> "[]".repeat(moreDims) <> init.map(Text(" =") <>> _)
        .getOrElse(Empty)
    )
}
