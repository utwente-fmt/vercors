package vct.col.ast

sealed trait Statement extends NodeFamily
trait ExtraStatement extends Statement

case class Eval(expr: Expr)(implicit val o: Origin) extends Statement with NoCheck
case class LocalDecl(local: Variable)(implicit val o: Origin) extends Statement with NoCheck
case class Return(result: Expr)(implicit val o: Origin) extends Statement with NoCheck
case class Assign(target: Expr, value: Expr)(implicit val o: Origin) extends Check(value.checkSubType(target.t)) with Statement
case class Block(statements: Seq[Statement])(implicit val o: Origin) extends Statement with NoCheck
case class Scope(locals: Seq[Variable], body: Statement)(implicit val o: Origin) extends Statement with NoCheck {
  override def enterCheckContext(context: CheckContext): CheckContext = context.withScope(locals.toSet)
}
case class Branch(branches: Seq[(Expr, Statement)])(implicit val o: Origin) extends Check(branches.flatMap(_._1.checkSubType(TBool()))) with Statement
case class Switch(expr: Expr, body: Statement)(implicit val o: Origin) extends Statement with NoCheck
case class Loop(init: Statement, cond: Expr, update: Statement, invariant: Expr, body: Statement)(implicit val o: Origin)
  extends Check(cond.checkSubType(TBool()), invariant.checkSubType(TResource())) with Statement

case class CatchClause(decl: Variable, body: Statement)(implicit val o: Origin) extends NodeFamily with NoCheck with Declarator {
  override def declarations: Seq[Declaration] = Seq(decl)
}
case class TryCatchFinally(body: Statement, after: Statement, catches: Seq[CatchClause])(implicit val o: Origin) extends Statement with NoCheck

case class Synchronized(obj: Expr, body: Statement)(implicit val o: Origin) extends Check(obj.checkClassType) with Statement

case class ParInvariant(decl: ParInvariantDecl, inv: Expr, content: Statement)(implicit val o: Origin) extends Check(inv.checkSubType(TResource())) with Statement with Declarator {
  override def declarations: Seq[Declaration] = Seq(decl)
}
case class ParAtomic(inv: Ref[ParInvariantDecl], content: Statement)(implicit val o: Origin) extends Statement {
  override def check(context: CheckContext): Seq[CheckError] =
    context.inScope(inv)
}
case class ParBarrier(block: Ref[ParBlockDecl], invs: Seq[Ref[ParInvariantDecl]], requires: Expr, ensures: Expr, content: Statement)(implicit val o: Origin) extends Statement {
  override def check(context: CheckContext): Seq[CheckError] =
    requires.checkSubType(TResource()) ++
      ensures.checkSubType(TResource()) ++
      context.inScope(block) ++
      invs.flatMap(context.inScope)
}
case class ParRegion(requires: Expr, ensures: Expr, blocks: Seq[ParBlock])(implicit val o: Origin)
  extends Check(requires.checkSubType(TResource()), ensures.checkSubType(TResource())) with Statement with Declarator {
  override def declarations: Seq[Declaration] = blocks.map(_.decl)
}
case class IterVariable(variable: Variable, from: Expr, to: Expr)(implicit val o: Origin) extends Check(from.checkSubType(TInt()), to.checkSubType(TInt())) with NodeFamily
case class ParBlock(decl: ParBlockDecl, after: Seq[Ref[ParBlockDecl]], iters: Seq[IterVariable], requires: Expr, ensures: Expr, content: Statement)(implicit val o: Origin)
  extends Check(requires.checkSubType(TResource()), ensures.checkSubType(TResource())) with NodeFamily with Declarator {
  override def declarations: Seq[Declaration] = iters.map(_.variable)
}

case class Throw(e: Expr)(implicit val o: Origin) extends Check(e.checkClassType) with Statement

case class DefaultCase()(implicit val o: Origin) extends Statement with NoCheck
case class Case(pattern: Expr)(implicit val o: Origin) extends Statement with NoCheck
case class Label(decl: LabelDecl)(implicit val o: Origin) extends Statement with NoCheck
case class Goto(lbl: Ref[LabelDecl])(implicit val o: Origin) extends Statement {
  override def check(context: CheckContext): Seq[CheckError] =
    context.currentApplicable.get.body.get.transSubnodes.collectFirst {
      case label: LabelDecl if label == lbl.decl => label
    } match {
      case Some(_) => Seq()
      case None => Seq(OutOfScopeError(lbl))
    }
}

case class Exhale(res: Expr)(val blame: ExhaleBlame)(implicit val o: Origin) extends Check(res.checkSubType(TResource())) with Statement
case class Assert(assn: Expr)(val blame: AssertBlame)(implicit val o: Origin) extends Check(assn.checkSubType(TBool())) with Statement
case class Refute(assn: Expr)(implicit val o: Origin) extends Check(assn.checkSubType(TBool())) with Statement
case class Inhale(res: Expr)(implicit val o: Origin) extends Check(res.checkSubType(TResource())) with Statement
case class Assume(assn: Expr)(implicit val o: Origin) extends Check(assn.checkSubType(TBool())) with Statement

case class SpecIgnoreStart()(implicit val o: Origin) extends Statement with NoCheck
case class SpecIgnoreEnd()(implicit val o: Origin) extends Statement with NoCheck

case class Wait(obj: Expr)(implicit val o: Origin) extends Check(obj.checkClassType) with Statement
case class Notify(obj: Expr)(implicit val o: Origin) extends Check(obj.checkClassType) with Statement
case class Fork(runnable: Expr)(implicit val o: Origin) extends Check(runnable.checkClassType) with Statement
case class Join(runnable: Expr)(implicit val o: Origin) extends Check(runnable.checkClassType) with Statement
case class Lock(obj: Expr)(implicit val o: Origin) extends Check(obj.checkClassType) with Statement
case class Unlock(obj: Expr)(implicit val o: Origin) extends Check(obj.checkClassType) with Statement

case class Fold(pred: Expr)(implicit val o: Origin) extends Statement {
  override def check(context: CheckContext): Seq[CheckError] = pred.checkSubType(TResource())
}
case class Unfold(pred: Expr)(implicit val o: Origin) extends Statement {
  override def check(context: CheckContext): Seq[CheckError] = pred.checkSubType(TResource())
}

case class WandCreate(statements: Seq[Statement])(implicit val o: Origin) extends Statement with NoCheck
case class WandQed(wand: Expr)(implicit val o: Origin) extends Check(wand.checkSubType(TResource())) with Statement
case class WandApply(wand: Expr)(implicit val o: Origin) extends Check(wand.checkSubType(TResource())) with Statement
case class WandUse(pred: Expr)(implicit val o: Origin) extends Check(pred.checkSubType(TResource())) with Statement

case class ModelCreate(target: Expr, model: Ref[Model], process: Expr)(implicit val o: Origin)
  extends Check(process.checkSubType(TProcess())) with Statement
case class ModelDestroy(model: Expr)(implicit val o: Origin) extends Check(model.checkModelThen()) with Statement
case class ModelSplitInto(model: Expr, leftPerm: Expr, left: Expr, rightPerm: Expr, right: Expr)(implicit val o: Origin)
  extends Check(leftPerm.checkSubType(TRational()), rightPerm.checkSubType(TRational()),
                left.checkSubType(TProcess()), right.checkSubType(TProcess())) with Statement
case class ModelMergeFrom(model: Expr, leftPerm: Expr, left: Expr, rightPerm: Expr, right: Expr)(implicit val o: Origin)
  extends Check(leftPerm.checkSubType(TRational()), rightPerm.checkSubType(TRational()),
                left.checkSubType(TProcess()), right.checkSubType(TProcess())) with Statement
case class ModelChoose(model: Expr, perm: Expr, choiceProcess: Expr, choice: Expr)(implicit val o: Origin)
  extends Check(model.checkModelThen(), perm.checkSubType(TRational()),
                choiceProcess.checkSubType(TProcess()), choice.checkSubType(TProcess())) with Statement
case class ModelDo(model: Expr, perm: Expr, after: Expr, action: Expr)(implicit val o: Origin)
  extends Check(model.checkModelThen(), perm.checkSubType(TRational()),
                after.checkSubType(TProcess()), action.checkSubType(TProcess())) with Statement

case class Havoc(loc: Expr)(implicit val o: Origin) extends Statement with NoCheck

case class Break(label: Option[Ref[LabelDecl]])(implicit val o: Origin) extends Statement with NoCheck
case class Continue(label: Option[Ref[LabelDecl]])(implicit val o: Origin) extends Statement with NoCheck