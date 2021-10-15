package vct.col.resolve

import vct.col.ast._
import vct.result.VerificationResult.UserError

case object Spec {
  case class BuiltinArgumentCountError(obj: Expr, expected: Int) extends UserError {
    override def text: String =
      obj.o.messageInContext(s"Builtin method invocation has wrong argument count: expected $expected argument(s).")
    override def code: String = "builtinArgCount"
  }

  def builtinField(obj: Expr, field: String): Option[BuiltinField] = {
    implicit val o: Origin = obj.o
    Some(BuiltinField((obj.t.mimics, field) match {
      case (TArray(_), "length") => Length(_)

      case (_: CollectionType, "isEmpty") => Empty(_)
      case (_: CollectionType, "size") => Size(_)

      case (TSeq(_), "head") => Head(_)
      case (TSeq(_), "tail") => Tail(_)

      case (TMap(_, _), "values") => MapValueSet(_)
      case (TMap(_, _), "items") => MapItemSet(_)
      case (TMap(_, _), "keys") => MapKeySet(_)

      case (TOption(_), "get") => OptGet(_)

      case (TTuple(_), "fst") => TupGet(_, 0)
      case (TTuple(_), "snd") => TupGet(_, 1)

      case _ => return None
    }))
  }

  def argCount(n: Int)(f: Expr => Seq[Expr] => Expr): Expr => Seq[Expr] => Expr =
    obj => args =>
      if(args.size == n) f(obj)(args)
      else throw BuiltinArgumentCountError(obj, n)

  def builtinInstanceMethod(obj: Expr, method: String): Option[BuiltinInstanceMethod] = {
    implicit val o: Origin = obj.o
    Some(BuiltinInstanceMethod((obj.t.mimics, method) match {
      case (t @ TNotAValue(), _) => (t.decl.get, method) match {
        case (RefModel(model), "create") => _ => _ => ModelNew(model.ref)
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
      case (TMap(_, _), "get") => argCount(1)(obj => args => MapGet(obj, args.head))
      case (TMap(_, _), "equals") => argCount(1)(obj => args => MapEq(obj, args.head))
      case (TMap(_, _), "disjoint") => argCount(1)(obj => args => MapDisjoint(obj, args.head))

      case (TOption(_), "getOrElse") => argCount(1)(obj => args => OptGetOrElse(obj, args.head))

      case (_, _) => return None
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
