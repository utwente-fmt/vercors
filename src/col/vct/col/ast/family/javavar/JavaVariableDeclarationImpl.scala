package vct.col.ast.family.javavar

import vct.col.ast.JavaVariableDeclaration
import vct.col.print._
import vct.col.ast.ops.{
  JavaVariableDeclarationOps,
  JavaVariableDeclarationFamilyOps,
}

trait JavaVariableDeclarationImpl[G]
    extends JavaVariableDeclarationOps[G]
    with JavaVariableDeclarationFamilyOps[G] {
  this: JavaVariableDeclaration[G] =>
  override def layout(implicit ctx: Ctx): Doc =
    Group(
      Text(name) <> "[]".repeat(moreDims) <> init.map(Text(" =") <>> _)
        .getOrElse(Empty)
    )
}
