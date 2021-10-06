package vct.col.resolve

import vct.col.ast
import vct.col.ast.{ADTFunction, CovariantType, Expr, ExtraType, LeafType, PVLConstructor, PVLNamedType, TAny, TBoundedInt, TClass, TFraction, TInt, TNotAValue, TNull, TRational, TResource, TZFraction, Class}

case object PVL {
  def findConstructor(cls: ast.Class, args: Seq[Expr]): Option[PVLConstructor] =
    cls.declarations.collectFirst {
      case cons: PVLConstructor if Java.compat(args, cons.args) => cons
    }

  def findTypeName(name: String, ctx: TypeResolutionContext): Option[PVLTypeNameTarget] =
    ctx.stack.flatten.collectFirst {
      case target: PVLTypeNameTarget if target.name == name => target
    }

  def findName(name: String, ctx: ReferenceResolutionContext): Option[PVLNameTarget] =
    ctx.stack.flatten.collectFirst {
      case target: PVLNameTarget if target.name == name => target
    }

  def findDerefOfClass(decl: Class, name: String): Option[PVLDerefTarget] =
    decl.declarations.flatMap(Referrable.from).collectFirst {
      case ref: RefInstanceMethod if ref.name == name => ref
      case ref: RefInstancePredicate if ref.name == name => ref
      case ref: RefInstanceFunction if ref.name == name => ref
      case ref: RefField if ref.name == name => ref
    }

  def findDeref(obj: Expr, name: String, ctx: ReferenceResolutionContext): Option[PVLDerefTarget] =
    obj.t match {
      case t: TNotAValue => t.decl.get match {
        case _ => Spec.builtinField(obj, name)
      }
      case t: PVLNamedType => t.ref.get match { // should just use mimics here?
        case RefAxiomaticDataType(decl) => decl.decls.flatMap(Referrable.from).collectFirst {
          case ref: RefADTFunction if ref.name == name => ref
        }
        case RefModel(decl) => decl.declarations.flatMap(Referrable.from).collectFirst {
          case ref: RefModelField if ref.name == name => ref
          case ref: RefModelAction if ref.name == name => ref
          case ref: RefModelProcess if ref.name == name => ref
        }.orElse(Spec.builtinInstanceMethod(obj, name))
        case RefClass(decl) => findDerefOfClass(decl, name)
      }
      case TClass(ref) => findDerefOfClass(ref.decl, name)
      case _ => Spec.builtinField(obj, name).orElse(Spec.builtinInstanceMethod(obj, name))
    }

  def resolveInvocation(obj: Expr, ctx: ReferenceResolutionContext): PVLInvocationTarget =
    obj.t match {
      case t @ TNotAValue() => t.decl.get match {
        case target: PVLInvocationTarget => target
        case _ => throw NotApplicable(obj)
      }
      case _ => throw NotApplicable(obj)
    }
}
