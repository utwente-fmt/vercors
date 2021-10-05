package vct.col.resolve

import vct.col.ast._

case object Spec {
  def builtinField(obj: Expr, field: String): Option[BuiltinField] = {
    implicit val o: Origin = obj.o
    Some(BuiltinField((obj.t, field) match {
      case (TArray(_), "length") => Length(_)
      case _ => return None
    }))
  }

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

  def findModelField(name: String, ctx: ReferenceResolutionContext): Option[ModelField] =
    ctx.stack.flatten.collectFirst {
      case ref @ RefModelField(decl) if ref.name == name => decl
    }

  def findParBlock(name: String, ctx: ReferenceResolutionContext): Option[ParBlockDecl] =
    ctx.stack.flatten.collectFirst {
      case ref @ RefParBlockDecl(decl) if ref.name == name => decl
    }

  def findParInvariant(name: String, ctx: ReferenceResolutionContext): Option[ParInvariantDecl] =
    ctx.stack.flatten.collectFirst {
      case ref @ RefParInvariantDecl(decl) if ref.name == name => decl
    }
}
