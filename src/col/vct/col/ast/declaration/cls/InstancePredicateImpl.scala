package vct.col.ast.declaration.cls

import vct.col.ast.InstancePredicate
import vct.col.ast.declaration.category.AbstractPredicateImpl
import vct.col.print._

trait InstancePredicateImpl[G] extends ClassDeclarationImpl[G] with AbstractPredicateImpl[G] { this: InstancePredicate[G] =>
  override def layout(implicit ctx: Ctx): Doc = Group(
    Text("resource") <+> ctx.name(this) <> "(" <> Doc.args(args) <> ")" <>
      body.map(Text(" =") <>> _).getOrElse(Text(";"))
  )
}