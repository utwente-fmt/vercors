package vct.col.resolve

import vct.col.ast._
import vct.col.origin._
import vct.col.ref.Ref
import vct.result.VerificationResult.UserError

case object Spec {
  def getContract[G](target: Referrable[G], blame: Node[G]): ApplicableContract[G] = target match {
    case RefFunction(decl) => decl.contract
    case RefProcedure(decl) => decl.contract
    case RefInstanceFunction(decl) => decl.contract
    case RefInstanceMethod(decl) => decl.contract
    case RefCDeclaration(decls, _) => decls.contract
    case RefCGlobalDeclaration(decls, _) => decls.decl.contract
//    case RefCFunctionDefinition(decl) =>
    case RefJavaConstructor(decl) => decl.contract
    case RefJavaMethod(decl) => decl.contract
    case RefPVLConstructor(decl) => decl.contract
    case other => throw NoGivenYields(blame)
  }

  def resolveGiven[G](givenMap: Seq[(Ref[G, Variable[G]], Expr[G])], target: Referrable[G], blame: Node[G]): Unit =
    if(givenMap.nonEmpty) {
      val contract = getContract(target, blame)
      val args = contract.givenArgs.flatMap(Referrable.from)
      givenMap.foreach {
        case (arg, _) => arg.tryResolve(name => args.collectFirst {
          case ref: RefVariable[G] if ref.name == name => ref.decl
        }.getOrElse(throw NoSuchNameError("'given' argument", name, blame)))
      }
    }

  def resolveYields[G](ctx: ReferenceResolutionContext[G], yields: Seq[(Ref[G, Variable[G]], Ref[G, Variable[G]])], target: Referrable[G], blame: Node[G]): Unit =
    if(yields.nonEmpty) {
      val contract = getContract(target, blame)
      val args = contract.yieldsArgs.flatMap(Referrable.from)
      yields.foreach {
        case (tgt, res) =>
          tgt.tryResolve(name => Spec.findLocal(name, ctx).getOrElse(throw NoSuchNameError("local", name, blame)))
          res.tryResolve(name => args.collectFirst {
            case ref: RefVariable[G] if ref.name == name => ref.decl
          }.getOrElse(throw NoSuchNameError("'yields' argument", name, blame)))
      }
    }

  case class BuiltinArgumentCountError(obj: Expr[_], expected: Int) extends UserError {
    override def text: String =
      obj.o.messageInContext(s"Builtin method invocation has wrong argument count: expected $expected argument(s).")
    override def code: String = "builtinArgCount"
  }

  def builtinField[G](obj: Expr[G], field: String, blame: Blame[BuiltinError]): Option[BuiltinField[G]] = {
    implicit val o: Origin = obj.o
    Some(BuiltinField((obj.t, field) match {
      case (TArray(_), "length") => Length(_)(blame)

      case (_: SizedType[G], "isEmpty") => Empty(_)
      case (_: SizedType[G], "size") => Size(_)

      case (TSeq(_), "head") => Head(_)(blame)
      case (TSeq(_), "tail") => Tail(_)

      case (TMap(_, _), "values") => MapValueSet(_)
      case (TMap(_, _), "items") => MapItemSet(_)
      case (TMap(_, _), "keys") => MapKeySet(_)

      case (TOption(_), "get") => OptGet(_)(blame)

      case (TTuple(_), "fst") => TupGet(_, 0)
      case (TTuple(_), "snd") => TupGet(_, 1)

      case (TEither(_, _), "left") => GetLeft(_)(blame)
      case (TEither(_, _), "right") => GetRight(_)(blame)
      case (TEither(_, _), "isLeft") => IsLeft(_)
      case (TEither(_, _), "isRight") => IsRight(_)

      case _ => return None
    }))
  }

  def argCount[G](n: Int)(f: Expr[G] => Seq[Expr[G]] => Expr[G]): Expr[G] => Seq[Expr[G]] => Expr[G] =
    obj => args =>
      if(args.size == n) f(obj)(args)
      else throw BuiltinArgumentCountError(obj, n)

  def builtinInstanceMethod[G](obj: Expr[G], method: String, blame: Blame[BuiltinError]): Option[BuiltinInstanceMethod[G]] = {
    implicit val o: Origin = obj.o
    Some(BuiltinInstanceMethod((obj.t, method) match {
      case (t: TNotAValue[G], _) => (t.decl.get, method) match {
        case (RefModel(model), "create") => _ => _ => ModelNew(model.ref)
        case (_, _) => return None
      }

      case (TModel(_), "state") => argCount(2)(obj => args => ModelState(obj, args(0), args(1)))
      case (TModel(_), "abstractState") => argCount(1)(obj => args => ModelAbstractState(obj, args.head))
      case (TModel(_), "create") => argCount(1)(obj => args => ModelCreate(obj, args.head))
      case (TModel(_), "destroy") => argCount(0)(obj => _ => ModelDestroy(obj))
      case (TModel(_), "split") => argCount(4)(obj => args => ModelSplit(obj, args(0), args(1), args(2), args(3)))
      case (TModel(_), "merge") => argCount(4)(obj => args => ModelMerge(obj, args(0), args(1), args(2), args(3)))
      case (TModel(_), "choose") => argCount(3)(obj => args => ModelChoose(obj, args(0), args(1), args(2)))

      case (TSeq(_), "removeAt") => argCount(1)(obj => args => RemoveAt(obj, args.head))

      case (TMap(_, _), "add") => argCount(2)(obj => args => MapCons(obj, args(0), args(1)))
      case (TMap(_, _), "remove") => argCount(1)(obj => args => MapRemove(obj, args.head))
      case (TMap(_, _), "get") => argCount(1)(obj => args => MapGet(obj, args.head)(blame))
      case (TMap(_, _), "equals") => argCount(1)(obj => args => MapEq(obj, args.head))
      case (TMap(_, _), "disjoint") => argCount(1)(obj => args => MapDisjoint(obj, args.head))

      case (TOption(_), "getOrElse") => argCount(1)(obj => args => OptGetOrElse(obj, args.head))

      case (_, _) => return None
    }))
  }

  def findLabel[G](name: String, ctx: ReferenceResolutionContext[G]): Option[LabelDecl[G]] =
    ctx.stack.flatten.collectFirst {
      case ref @ RefLabelDecl(decl) if ref.name == name => decl
    }

  def findLocal[G](name: String, ctx: ReferenceResolutionContext[G]): Option[Variable[G]] =
    ctx.stack.flatten.collectFirst {
      case ref @ RefVariable(decl) if ref.name == name => decl
    }

  def findClass[G](name: String, ctx: TypeResolutionContext[G]): Option[Class[G]] =
    ctx.stack.flatten.collectFirst {
      case ref @ RefClass(decl) if ref.name == name => decl
    }

  def findModelField[G](name: String, ctx: ReferenceResolutionContext[G]): Option[ModelField[G]] =
    ctx.stack.flatten.collectFirst {
      case ref @ RefModelField(decl) if ref.name == name => decl
    }

  def findParBlock[G](name: String, ctx: ReferenceResolutionContext[G]): Option[ParBlockDecl[G]] =
    ctx.stack.flatten.collectFirst {
      case ref @ RefParBlockDecl(decl) if ref.name == name => decl
    }

  def findParInvariant[G](name: String, ctx: ReferenceResolutionContext[G]): Option[ParInvariantDecl[G]] =
    ctx.stack.flatten.collectFirst {
      case ref @ RefParInvariantDecl(decl) if ref.name == name => decl
    }

  def findAdt[G](ctx: ReferenceResolutionContext[G], name: String): Option[AxiomaticDataType[G]] =
    ctx.stack.flatten.collectFirst {
      case ref @ RefAxiomaticDataType(decl) if ref.name == name => decl
    }

  def findAdtFunction[G](decl: AxiomaticDataType[G], name: String): Option[ADTFunction[G]] =
    decl.decls.flatMap(Referrable.from).collectFirst {
      case ref @ RefADTFunction(f) if ref.name == name => f
    }
}
