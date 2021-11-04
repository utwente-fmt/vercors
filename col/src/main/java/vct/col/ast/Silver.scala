package vct.col.ast

case class SilverPredicateAccess(ref: Ref[Predicate], args: Seq[Expr], perm: Expr)(implicit val o: Origin)
  extends Coercing with NodeFamily {
  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): SilverPredicateAccess =
    SilverPredicateAccess(ref, args, resolver(perm, TRational()))
}

sealed trait SilverExpr extends ExtraExpr
case class SilverDeref(obj: Expr, field: Ref[SilverField])(val blame: Blame[InsufficientPermission])(implicit val o: Origin)
  extends SilverExpr with HeapDeref with Coercing {
  override def t: Type = field.decl.t

  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): SilverDeref =
    SilverDeref(resolver(obj, TRef()), field)(blame)
}
sealed trait SilverResource extends SilverExpr {
  override def t: Type = TResource()
}
case class SilverPerm(obj: Expr, field: Ref[SilverField], perm: Expr)(implicit val o: Origin) extends Coercing with SilverResource {
  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Node =
    SilverPerm(resolver(obj, TRef()), field, resolver(perm, TRational()))
}
case class SilverPredPerm(access: SilverPredicateAccess)(implicit val o: Origin) extends SilverResource with NoCheck
case class SilverUnfolding(access: SilverPredicateAccess, body: Expr)(implicit val o: Origin) extends SilverExpr with NoCheck {
  override def t: Type = body.t
}
case class SilverCurFieldPerm(obj: Expr, field: Ref[SilverField])(implicit val o: Origin) extends Coercing with SilverExpr {
  override def t: Type = TRational()
  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): SilverCurFieldPerm =
    SilverCurFieldPerm(resolver(obj, TRef()), field)
}
case class SilverCurPredPerm(ref: Ref[Predicate], args: Seq[Expr])(implicit val o: Origin) extends SilverExpr with NoCheck {
  override def t: Type = TRational()
}

sealed trait SilverStatement extends ExtraStatement
case class SilverUnfold(access: SilverPredicateAccess)(val blame: Blame[SilverUnfoldFailed])(implicit val o: Origin) extends SilverStatement with NoCheck
case class SilverFold(access: SilverPredicateAccess)(val blame: Blame[SilverFoldFailed])(implicit val o: Origin) extends SilverStatement with NoCheck
case class SilverWhile(cond: Expr, invariant: Expr, body: Statement)(val blame: Blame[SilverWhileInvariantFailure])(implicit val o: Origin)
  extends Coercing with SilverStatement {
  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Node =
    SilverWhile(resolver(cond, TBool()), resolver(invariant, TResource()), body)(blame)
}
case class SilverIf(cond: Expr, whenTrue: Statement, whenFalse: Statement)(implicit val o: Origin)
  extends Coercing with SilverStatement {
  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Node =
    SilverIf(resolver(cond, TBool()), whenTrue, whenFalse)
}
case class SilverNewRef(v: Ref[Variable], fields: Seq[Ref[SilverField]])(implicit val o: Origin) extends SilverStatement with NoCheck

sealed trait SilverAssign extends SilverStatement
case class SilverFieldAssign(obj: Expr, field: Ref[SilverField], value: Expr)(val blame: Blame[SilverAssignFailed])(implicit val o: Origin)
  extends Coercing with SilverAssign {
  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Node =
    SilverFieldAssign(resolver(obj, TRef()), field, resolver(value, field.decl.t))(blame)
}
case class SilverLocalAssign(v: Ref[Variable], value: Expr)(implicit val o: Origin)
  extends Coercing with SilverAssign {
  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Node =
    SilverLocalAssign(v, resolver(value, v.decl.t))
}

sealed abstract class SilverDeclaration extends ExtraGlobalDeclaration
class SilverField(val t: Type)(implicit val o: Origin) extends SilverDeclaration with NoCheck
