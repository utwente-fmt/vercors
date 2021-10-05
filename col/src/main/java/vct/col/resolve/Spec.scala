package vct.col.resolve

import vct.col.ast.{Declaration, LabelDecl, Variable, Class}

case object Spec {
  def findLabel(name: String, ctx: ReferenceResolutionContext): Option[LabelDecl] =
    ctx.stack.flatten.collectFirst {
      case ref @ RefLabelDecl(decl) if ref.name == name => decl
    }

  def findLocal(name: String, ctx: ReferenceResolutionContext): Option[Variable] =
    ctx.stack.flatten.collectFirst {
      case ref @ RefVariable(decl) if ref.name == name => decl
    }

  def findClass(name: String, ctx: TypeResolutionContext): Option[Class] =
    ctx.stack.flatten.collectFirst {
      case ref @ RefClass(decl) if ref.name == name => decl
    }
}
