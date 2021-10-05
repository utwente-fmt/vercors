package vct.col.resolve

import vct.col.ast.{ADTFunction, CovariantType, Expr, ExtraType, LeafType, PVLNamedType, TAny, TBoundedInt, TClass, TFraction, TInt, TNotAValue, TNull, TRational, TResource, TZFraction}

case object PVL {
  def findTypeName(name: String, ctx: TypeResolutionContext): Option[PVLTypeNameTarget] =
    ctx.stack.flatten.collectFirst {
      case target: PVLTypeNameTarget if target.name == name => target
    }

  def findName(name: String, ctx: ReferenceResolutionContext): Option[PVLNameTarget] =
    ctx.stack.flatten.collectFirst {
      case target: PVLNameTarget if target.name == name => target
    }

  def findDeref(obj: Expr, name: String, ctx: ReferenceResolutionContext): Option[PVLDerefTarget] =
    obj.t match {
      case t: TNotAValue => t.decl.get match {
        case
      }
      case t: PVLNamedType => t.ref.get match {
        case RefAxiomaticDataType(decl) => decl.decls.flatMap(Referrable.from).collectFirst {
          case ref: RefADTFunction if ref.name == name => ref
        }
        case RefModel(decl) => decl.declarations.flatMap(Referrable.from).collectFirst {
          case ref: RefModelField if ref.name == name => ref
          case ref: RefModelAction if ref.name == name => ref
          case ref: RefModelProcess if ref.name == name => ref
        }
        case RefClass(decl) => decl.declarations.flatMap(Referrable.from).collectFirst {
          case ref: RefInstanceMethod if ref.name == name => ref
          case ref: RefInstancePredicate if ref.name == name => ref
          case ref: RefInstanceFunction if ref.name == name => ref
          case ref: RefField if ref.name == name => ref
        }
      }
      case _ => throw HasNoFields(obj)
    }
}
