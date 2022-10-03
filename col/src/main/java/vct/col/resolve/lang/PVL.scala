package vct.col.resolve.lang

import vct.col.ast._
import vct.col.ast.`type`.TFloats
import vct.col.origin._
import vct.col.ref.Ref
import vct.col.resolve.ctx._

case object PVL {
  def findConstructor[G](t: Type[G], args: Seq[Expr[G]]): Option[PVLConstructorTarget[G]] =
    t match {
      case TClass(Ref(cls)) =>
        val resolvedCons = cls.declarations.collectFirst {
          case cons: PVLConstructor[G] if Util.compat(args, cons.args) => RefPVLConstructor(cons)
        }

        args match {
          case Nil => resolvedCons.orElse(Some(ImplicitDefaultPVLConstructor()))
          case _ => resolvedCons
        }
      case TModel(Ref(model)) if args.isEmpty => Some(RefModel(model))
      case _ => None
    }

  def findTypeName[G](name: String, ctx: TypeResolutionContext[G]): Option[PVLTypeNameTarget[G]] =
    ctx.stack.flatten.collectFirst {
      case target: PVLTypeNameTarget[G] if target.name == name => target
    }

  def findName[G](name: String, ctx: ReferenceResolutionContext[G]): Option[PVLNameTarget[G]] =
    ctx.stack.flatten.collectFirst {
      case target: PVLNameTarget[G] if target.name == name => target
    }

  def findDerefOfClass[G](decl: Class[G], name: String): Option[PVLDerefTarget[G]] =
    decl.declarations.flatMap(Referrable.from).collectFirst {
      case ref: RefField[G] if ref.name == name => ref
    }

  def findDeref[G](obj: Expr[G], name: String, ctx: ReferenceResolutionContext[G], blame: Blame[BuiltinError]): Option[PVLDerefTarget[G]] =
    obj.t match {
      case _: TNotAValue[G] => Spec.builtinField(obj, name, blame)
      case TModel(ref) => ref.decl.declarations.flatMap(Referrable.from).collectFirst {
        case ref: RefModelField[G] if ref.name == name => ref
      }
      case TClass(ref) => findDerefOfClass(ref.decl, name)
      case _ => Spec.builtinField(obj, name, blame)
    }

  def findInstanceMethod[G](obj: Expr[G], method: String, args: Seq[Expr[G]], typeArgs: Seq[Type[G]], blame: Blame[BuiltinError]): Option[PVLInvocationTarget[G]] =
    obj.t match {
      case t: TNotAValue[G] => t.decl.get match {
        case RefAxiomaticDataType(decl) => decl.declarations.flatMap(Referrable.from).collectFirst {
          case ref: RefADTFunction[G] if ref.name == method => ref
        }
        case _ => Spec.builtinInstanceMethod(obj, method, blame)
      }
      case TModel(ref) => ref.decl.declarations.flatMap(Referrable.from).collectFirst {
        case ref: RefModelAction[G] if ref.name == method => ref
        case ref: RefModelProcess[G] if ref.name == method => ref
      }.orElse(Spec.builtinInstanceMethod(obj, method, blame))
      case TClass(ref) => ref.decl.declarations.flatMap(Referrable.from).collectFirst {
        case ref: RefInstanceFunction[G] if ref.name == method && Util.compat(args, typeArgs, ref.decl) => ref
        case ref: RefInstanceMethod[G] if ref.name == method && Util.compat(args, typeArgs, ref.decl) => ref
        case ref: RefInstancePredicate[G] if ref.name == method && Util.compat(args, ref.decl.args) => ref
      }
      case _ => PVL.builtinInstanceMethod(obj, method, args).orElse(Spec.builtinInstanceMethod(obj, method, blame))
    }

  def findMethod[G](method: String, args: Seq[Expr[G]], typeArgs: Seq[Type[G]], ctx: ReferenceResolutionContext[G]): Option[PVLInvocationTarget[G]] =
    ctx.stack.flatten.collectFirst {
      case ref: RefFunction[G] if ref.name == method && Util.compat(args, typeArgs, ref.decl) => ref
      case ref: RefProcedure[G] if ref.name == method && Util.compat(args, typeArgs, ref.decl) => ref
      case ref: RefPredicate[G] if ref.name == method && Util.compat(args, ref.decl.args) => ref
      case ref: RefInstanceFunction[G] if ref.name == method && Util.compat(args, typeArgs, ref.decl) => ref
      case ref: RefInstanceMethod[G] if ref.name == method && Util.compat(args, typeArgs, ref.decl) => ref
      case ref: RefInstancePredicate[G] if ref.name == method && Util.compat(args, ref.decl.args) => ref
      case ref: RefADTFunction[G] if ref.name == method && Util.compat(args, ref.decl.args) => ref
      case ref: RefModelProcess[G] if ref.name == method && Util.compat(args, ref.decl.args) => ref
      case ref: RefModelAction[G] if ref.name == method && Util.compat(args, ref.decl.args) => ref
    }

  def builtinInstanceMethod[G](obj: Expr[G], method: String, args: Seq[Expr[G]]): Option[PVLBuiltinInstanceMethod[G]] = {
    implicit val o: Origin = obj.o
    Some((obj.t, method, args) match {
      case (TFloat(_, _), "toFloat32", Seq()) => PVLBuiltinInstanceMethod[G](obj => _ => CastFloat(obj, PVL.float32))
      case (TFloat(_, _), "toFloat64", Seq()) => PVLBuiltinInstanceMethod[G](obj => _ => CastFloat(obj, PVL.float64))
      case _ => return None
    })
  }

  def float32[G](implicit o: Origin = DiagnosticOrigin): TFloat[G] = TFloats.ieee754_32bit
  def float64[G](implicit o: Origin = DiagnosticOrigin): TFloat[G] = TFloats.ieee754_64bit
}
