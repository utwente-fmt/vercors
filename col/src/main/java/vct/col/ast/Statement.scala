package vct.col.ast

import vct.col.resolve.ResolveReferences

sealed trait Statement extends NodeFamily
trait ExtraStatement extends Statement

case class Eval(expr: Expr)(implicit val o: Origin) extends Statement
case class LocalDecl(local: Variable)(implicit val o: Origin) extends Statement
case class Return(result: Expr)(implicit val o: Origin) extends Statement
case class Assign(target: Expr, value: Expr)(implicit val o: Origin) extends Statement
case class Block(statements: Seq[Statement])(implicit val o: Origin) extends Statement
case class Scope(locals: Seq[Variable], body: Statement)(implicit val o: Origin) extends Statement {
  override def enterCheckContext(context: CheckContext): CheckContext = context.withScope((locals ++ ResolveReferences.scanScope(body)).toSet)
}

sealed trait LoopContract extends NodeFamily
case class LoopInvariant(invariant: Expr)(implicit val o: Origin) extends LoopContract
case class IterationContract(requires: Expr, ensures: Expr)(implicit val o: Origin) extends LoopContract

case class Branch(branches: Seq[(Expr, Statement)])(implicit val o: Origin) extends Statement
case class Switch(expr: Expr, body: Statement)(implicit val o: Origin) extends Statement
case class Loop(init: Statement, cond: Expr, update: Statement, contract: LoopContract, body: Statement)(implicit val o: Origin) extends Statement

case class CatchClause(decl: Variable, body: Statement)(implicit val o: Origin) extends NodeFamily with Declarator {
  override def declarations: Seq[Declaration] = Seq(decl)
}
case class TryCatchFinally(body: Statement, after: Statement, catches: Seq[CatchClause])(implicit val o: Origin) extends Statement

case class Synchronized(obj: Expr, body: Statement)(implicit val o: Origin) extends Statement

case class ParInvariant(decl: ParInvariantDecl, inv: Expr, content: Statement)(val blame: Blame[ParInvariantNotEstablished])(implicit val o: Origin) extends Statement with Declarator {
  override def declarations: Seq[Declaration] = Seq(decl)
}
case class ParAtomic(inv: Seq[Ref[ParInvariantDecl]], content: Statement)(implicit val o: Origin) extends Statement {
  override def check(context: CheckContext): Seq[CheckError] =
    inv.flatMap(context.checkInScope(this, _))
}
case class ParBarrier(block: Ref[ParBlockDecl], invs: Seq[Ref[ParInvariantDecl]], requires: Expr, ensures: Expr, content: Statement)(val blame: Blame[ParBarrierFailed])(implicit val o: Origin) extends Statement
case class ParRegion(requires: Expr, ensures: Expr, blocks: Seq[ParBlock])(val blame: Blame[ParRegionFailed])(implicit val o: Origin) extends Statement with Declarator {
  override def declarations: Seq[Declaration] = blocks.map(_.decl)
}
case class IterVariable(variable: Variable, from: Expr, to: Expr)(implicit val o: Origin) extends NodeFamily
case class ParBlock(decl: ParBlockDecl, after: Seq[Ref[ParBlockDecl]], iters: Seq[IterVariable], requires: Expr, ensures: Expr, content: Statement)(implicit val o: Origin)
  extends NodeFamily with Declarator {
  override def declarations: Seq[Declaration] = iters.map(_.variable)
}

case class VecBlock(iters: Seq[IterVariable], requires: Expr, ensures: Expr, content: Statement)(implicit val o: Origin)
  extends Statement with Declarator {
  override def declarations: Seq[Declaration] = iters.map(_.variable)
}

// PB: send and recv should receive some syntax updates: I think a built-in condition is useful (now collected from if
// statements) and the offset should be a constant, rather than an expression (since they need to be one-to-one).
// Perhaps we also shouldn't lean on labels, and instead name the statements themselves or so.
case class Send(resource: Expr, label: Ref[LabelDecl], offset: Expr)(implicit val o: Origin) extends Statement
case class Recv(resource: Expr, label: Ref[LabelDecl], offset: Expr)(implicit val o: Origin) extends Statement

case class DefaultCase()(implicit val o: Origin) extends Statement
case class Case(pattern: Expr)(implicit val o: Origin) extends Statement
case class Label(decl: LabelDecl)(implicit val o: Origin) extends Statement
case class Goto(lbl: Ref[LabelDecl])(implicit val o: Origin) extends Statement {
  override def check(context: CheckContext): Seq[CheckError] =
    context.currentApplicable.get.body.get.transSubnodes.collectFirst {
      case label: LabelDecl if label == lbl.decl => label
    } match {
      case Some(_) => Seq()
      case None => Seq(OutOfScopeError(this, lbl))
    }
}

case class Exhale(res: Expr)(val blame: Blame[ExhaleFailed])(implicit val o: Origin) extends Statement
case class Assert(res: Expr)(val blame: Blame[AssertFailed])(implicit val o: Origin) extends Statement
case class Refute(assn: Expr)(implicit val o: Origin) extends Statement
case class Inhale(res: Expr)(implicit val o: Origin) extends Statement
case class Assume(assn: Expr)(implicit val o: Origin) extends Statement

case class SpecIgnoreStart()(implicit val o: Origin) extends Statement
case class SpecIgnoreEnd()(implicit val o: Origin) extends Statement

case class Throw(obj: Expr)(implicit val o: Origin) extends Statement
case class Wait(obj: Expr)(implicit val o: Origin) extends Statement
case class Notify(obj: Expr)(implicit val o: Origin) extends Statement
case class Fork(obj: Expr)(implicit val o: Origin) extends Statement
case class Join(obj: Expr)(implicit val o: Origin) extends Statement
case class Lock(obj: Expr)(implicit val o: Origin) extends Statement
case class Unlock(obj: Expr)(implicit val o: Origin) extends Statement

case class Fold(res: Expr)(implicit val o: Origin) extends Statement
case class Unfold(res: Expr)(implicit val o: Origin) extends Statement

case class WandCreate(statements: Seq[Statement])(implicit val o: Origin) extends Statement
case class WandQed(res: Expr)(implicit val o: Origin) extends Statement
case class WandApply(res: Expr)(implicit val o: Origin) extends Statement
case class WandUse(res: Expr)(implicit val o: Origin) extends Statement

case class ModelDo(model: Expr, perm: Expr, after: Expr, action: Expr, impl: Statement)(implicit val o: Origin) extends Statement

case class Havoc(loc: Expr)(implicit val o: Origin) extends Statement

case class Break(label: Option[Ref[LabelDecl]])(implicit val o: Origin) extends Statement
case class Continue(label: Option[Ref[LabelDecl]])(implicit val o: Origin) extends Statement