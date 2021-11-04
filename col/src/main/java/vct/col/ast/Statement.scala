package vct.col.ast

import vct.col.resolve.ResolveReferences

sealed trait Statement extends NodeFamily
trait ExtraStatement extends Statement

trait CoercingStatement extends Statement with Coercing {
  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Statement
}

case class Eval(expr: Expr)(implicit val o: Origin) extends Statement with NoCheck
case class LocalDecl(local: Variable)(implicit val o: Origin) extends Statement with NoCheck
case class Return(result: Expr)(implicit val o: Origin) extends Statement with NoCheck
case class Assign(target: Expr, value: Expr)(implicit val o: Origin) extends CoercingStatement {
  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Statement =
    Assign(target, resolver(value, target.t))
}
case class Block(statements: Seq[Statement])(implicit val o: Origin) extends Statement with NoCheck
case class Scope(locals: Seq[Variable], body: Statement)(implicit val o: Origin) extends Statement with NoCheck {
  override def enterCheckContext(context: CheckContext): CheckContext = context.withScope((locals ++ ResolveReferences.scanScope(body)).toSet)
}

sealed trait LoopContract extends NodeFamily
case class LoopInvariant(invariant: Expr)(implicit val o: Origin) extends Coercing with LoopContract {
  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): LoopContract =
    LoopInvariant(resolver(invariant, TResource()))
}
case class IterationContract(requires: Expr, ensures: Expr)(implicit val o: Origin) extends Coercing with LoopContract {
  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Node =
    IterationContract(resolver(requires, TResource()), resolver(ensures, TResource()))
}

case class Branch(branches: Seq[(Expr, Statement)])(implicit val o: Origin) extends CoercingStatement {
  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Statement =
    Branch(branches.map {
      case (cond, body) => (resolver(cond, TBool()), body)
    })
}
case class Switch(expr: Expr, body: Statement)(implicit val o: Origin) extends Statement with NoCheck
case class Loop(init: Statement, cond: Expr, update: Statement, contract: LoopContract, body: Statement)(implicit val o: Origin) extends CoercingStatement {
  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Statement =
    Loop(init, resolver(cond, TBool()), update, contract, body)
}

case class CatchClause(decl: Variable, body: Statement)(implicit val o: Origin) extends NodeFamily with NoCheck with Declarator {
  override def declarations: Seq[Declaration] = Seq(decl)
}
case class TryCatchFinally(body: Statement, after: Statement, catches: Seq[CatchClause])(implicit val o: Origin) extends Statement with NoCheck

case class Synchronized(obj: Expr, body: Statement)(implicit val o: Origin) extends CoercingStatement {
  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Statement =
    Synchronized(resolver.cls(obj), body)
}

case class ParInvariant(decl: ParInvariantDecl, inv: Expr, content: Statement)(val blame: Blame[ParInvariantNotEstablished])(implicit val o: Origin) extends CoercingStatement with Declarator {
  override def declarations: Seq[Declaration] = Seq(decl)

  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Statement =
    ParInvariant(decl, resolver(inv, TResource()), content)(blame)
}
case class ParAtomic(inv: Seq[Ref[ParInvariantDecl]], content: Statement)(implicit val o: Origin) extends Statement {
  override def check(context: CheckContext): Seq[CheckError] =
    inv.flatMap(context.checkInScope(this, _))
}
case class ParBarrier(block: Ref[ParBlockDecl], invs: Seq[Ref[ParInvariantDecl]], requires: Expr, ensures: Expr, content: Statement)(val blame: Blame[ParBarrierFailed])(implicit val o: Origin) extends CoercingStatement {
  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Statement =
    ParBarrier(block, invs, resolver(requires, TResource()), resolver(ensures, TResource()), content)(blame)
}
case class ParRegion(requires: Expr, ensures: Expr, blocks: Seq[ParBlock])(val blame: Blame[ParRegionFailed])(implicit val o: Origin) extends CoercingStatement with Declarator {
  override def declarations: Seq[Declaration] = blocks.map(_.decl)

  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Statement =
    ParRegion(resolver(requires, TResource()), resolver(ensures, TResource()), blocks)(blame)
}
case class IterVariable(variable: Variable, from: Expr, to: Expr)(implicit val o: Origin) extends Coercing with NodeFamily {
  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): IterVariable =
    IterVariable(variable, resolver(from, TInt()), resolver(to, TInt()))
}
case class ParBlock(decl: ParBlockDecl, after: Seq[Ref[ParBlockDecl]], iters: Seq[IterVariable], requires: Expr, ensures: Expr, content: Statement)(implicit val o: Origin)
  extends Coercing with NodeFamily with Declarator {
  override def declarations: Seq[Declaration] = iters.map(_.variable)
  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Node =
    ParBlock(decl, after, iters, resolver(requires, TResource()), resolver(ensures, TResource()), content)
}

case class VecBlock(iters: Seq[IterVariable], requires: Expr, ensures: Expr, content: Statement)(implicit val o: Origin)
  extends CoercingStatement with Declarator {
  override def declarations: Seq[Declaration] = iters.map(_.variable)
  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Statement =
    VecBlock(iters, resolver(requires, TResource()), resolver(ensures, TResource()), content)
}

// PB: send and recv should receive some syntax updates: I think a built-in condition is useful (now collected from if
// statements) and the offset should be a constant, rather than an expression (since they need to be one-to-one).
// Perhaps we also shouldn't lean on labels, and instead name the statements themselves or so.
case class Send(resource: Expr, label: Ref[LabelDecl], offset: Expr)(implicit val o: Origin) extends Statement with NoCheck
case class Recv(resource: Expr, label: Ref[LabelDecl], offset: Expr)(implicit val o: Origin) extends Statement with NoCheck

case class DefaultCase()(implicit val o: Origin) extends Statement with NoCheck
case class Case(pattern: Expr)(implicit val o: Origin) extends Statement with NoCheck
case class Label(decl: LabelDecl)(implicit val o: Origin) extends Statement with NoCheck
case class Goto(lbl: Ref[LabelDecl])(implicit val o: Origin) extends Statement {
  override def check(context: CheckContext): Seq[CheckError] =
    context.currentApplicable.get.body.get.transSubnodes.collectFirst {
      case label: LabelDecl if label == lbl.decl => label
    } match {
      case Some(_) => Seq()
      case None => Seq(OutOfScopeError(this, lbl))
    }
}

abstract class BooleanAssnOp(f: Expr => Statement) extends CoercingStatement {
  def assn: Expr
  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Statement =
    f(resolver(assn, TBool()))
}

abstract class ResourceOp(f: Expr => Statement) extends CoercingStatement {
  def res: Expr
  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Statement =
    f(resolver(res, TResource()))
}

case class Exhale(res: Expr)(val blame: Blame[ExhaleFailed])(implicit val o: Origin) extends ResourceOp(Exhale(_)(blame)(o))
case class Assert(res: Expr)(val blame: Blame[AssertFailed])(implicit val o: Origin) extends ResourceOp(Assert(_)(blame)(o))
case class Refute(assn: Expr)(implicit val o: Origin) extends BooleanAssnOp(Refute(_)(o))
case class Inhale(res: Expr)(implicit val o: Origin) extends ResourceOp(Inhale(_)(o))
case class Assume(assn: Expr)(implicit val o: Origin) extends BooleanAssnOp(Assume(_)(o))

case class SpecIgnoreStart()(implicit val o: Origin) extends Statement with NoCheck
case class SpecIgnoreEnd()(implicit val o: Origin) extends Statement with NoCheck

abstract class ClassOp(f: Expr => Statement) extends CoercingStatement {
  def obj: Expr
  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Statement =
    f(resolver.cls(obj))
}

case class Throw(obj: Expr)(implicit val o: Origin) extends ClassOp(Throw(_)(o))
case class Wait(obj: Expr)(implicit val o: Origin) extends ClassOp(Wait(_)(o))
case class Notify(obj: Expr)(implicit val o: Origin) extends ClassOp(Notify(_)(o))
case class Fork(obj: Expr)(implicit val o: Origin) extends ClassOp(Fork(_)(o))
case class Join(obj: Expr)(implicit val o: Origin) extends ClassOp(Join(_)(o))
case class Lock(obj: Expr)(implicit val o: Origin) extends ClassOp(Lock(_)(o))
case class Unlock(obj: Expr)(implicit val o: Origin) extends ClassOp(Unlock(_)(o))

case class Fold(res: Expr)(implicit val o: Origin) extends ResourceOp(Fold(_)(o))
case class Unfold(res: Expr)(implicit val o: Origin) extends ResourceOp(Unfold(_)(o))

case class WandCreate(statements: Seq[Statement])(implicit val o: Origin) extends Statement with NoCheck
case class WandQed(res: Expr)(implicit val o: Origin) extends ResourceOp(WandQed(_)(o))
case class WandApply(res: Expr)(implicit val o: Origin) extends ResourceOp(WandApply(_)(o))
case class WandUse(res: Expr)(implicit val o: Origin) extends ResourceOp(WandUse(_)(o))

case class ModelDo(model: Expr, perm: Expr, after: Expr, action: Expr, impl: Statement)(implicit val o: Origin) extends CoercingStatement {
  override def coerce(resolver: ResolveCoercion)(implicit o: Origin, sc: ScopeContext): Statement =
    ModelDo(resolver.model(model), resolver(perm, TRational()), resolver(after, TProcess()), resolver(action, TProcess()), impl)
}

case class Havoc(loc: Expr)(implicit val o: Origin) extends Statement with NoCheck

case class Break(label: Option[Ref[LabelDecl]])(implicit val o: Origin) extends Statement with NoCheck
case class Continue(label: Option[Ref[LabelDecl]])(implicit val o: Origin) extends Statement with NoCheck