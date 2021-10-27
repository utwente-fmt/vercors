package vct.col.resolve

import vct.col.ast
import vct.col.ast.{ADTFunction, Class, CovariantType, Expr, ExtraType, LeafType, PVLConstructor, PVLNamedType, TAny, TBoundedInt, TClass, TFraction, TInt, TModel, TNotAValue, TNull, TRational, TResource, TZFraction, Type}

case object PVL {
  def findConstructor(cls: ast.Class, args: Seq[Expr]): Option[PVLConstructor] =
    cls.declarations.collectFirst {
      case cons: PVLConstructor if Util.compat(args, cons.args) => cons
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
      case ref: RefField if ref.name == name => ref
    }

  def findDeref(obj: Expr, name: String, ctx: ReferenceResolutionContext): Option[PVLDerefTarget] =
    obj.t.mimics match {
      case _: TNotAValue => Spec.builtinField(obj, name)
      case TModel(ref) => ref.decl.declarations.flatMap(Referrable.from).collectFirst {
        case ref: RefModelField if ref.name == name => ref
      }
      case TClass(ref) => findDerefOfClass(ref.decl, name)
      case _ => Spec.builtinField(obj, name)
    }

  def findInstanceMethod(obj: Expr, method: String, args: Seq[Expr], typeArgs: Seq[Type]): Option[PVLInvocationTarget] =
    obj.t.mimics match {
      case t: TNotAValue => t.decl.get match {
        case RefAxiomaticDataType(decl) => decl.declarations.flatMap(Referrable.from).collectFirst {
          case ref: RefADTFunction if ref.name == method => ref
        }
        case _ => Spec.builtinInstanceMethod(obj, method)
      }
      case TModel(ref) => ref.decl.declarations.flatMap(Referrable.from).collectFirst {
        case ref: RefModelAction if ref.name == method => ref
        case ref: RefModelProcess if ref.name == method => ref
      }.orElse(Spec.builtinInstanceMethod(obj, method))
      case TClass(ref) => ref.decl.declarations.flatMap(Referrable.from).collectFirst {
        case ref: RefInstanceFunction if ref.name == method && Util.compat(args, typeArgs, ref.decl) => ref
        case ref: RefInstanceMethod if ref.name == method && Util.compat(args, typeArgs, ref.decl) => ref
        case ref: RefInstancePredicate if ref.name == method && Util.compat(args, ref.decl.args) => ref
      }
      case _ => Spec.builtinInstanceMethod(obj, method)
    }

  def findMethod(method: String, args: Seq[Expr], typeArgs: Seq[Type], ctx: ReferenceResolutionContext): Option[PVLInvocationTarget] =
    ctx.stack.flatten.collectFirst {
      case ref: RefFunction if ref.name == method && Util.compat(args, typeArgs, ref.decl) => ref
      case ref: RefProcedure if ref.name == method && Util.compat(args, typeArgs, ref.decl) => ref
      case ref: RefPredicate if ref.name == method && Util.compat(args, ref.decl.args) => ref
      case ref: RefInstanceFunction if ref.name == method && Util.compat(args, typeArgs, ref.decl) => ref
      case ref: RefInstanceMethod if ref.name == method && Util.compat(args, typeArgs, ref.decl) => ref
      case ref: RefInstancePredicate if ref.name == method && Util.compat(args, ref.decl.args) => ref
      case ref: RefADTFunction if ref.name == method && Util.compat(args, ref.decl.args) => ref
      case ref: RefModelProcess if ref.name == method && Util.compat(args, ref.decl.args) => ref
      case ref: RefModelAction if ref.name == method && Util.compat(args, ref.decl.args) => ref
    }
}
