package vct.col.resolve

import vct.col.ast.{LabelDecl, Variable}

case object Spec {
  def findLabel(name: String, ctx: ReferenceResolutionContext): Option[LabelDecl] =
    ctx.stack.flatten.collectFirst {
      case ref @ RefLabelDecl(decl) if ref.name == name => decl
    }

  def findLocal(name: String, ctx: ReferenceResolutionContext): Option[Variable] =
    ctx.stack.flatten.collectFirst {
      case ref @ RefVariable(decl) if ref.name == name => decl
    }
}
