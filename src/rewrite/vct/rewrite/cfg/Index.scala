package vct.rewrite.cfg

import vct.col.ast._

case class GlobalIndex[G](indices: List[Index[G]]) {

  def enter_scope(statement: Statement[G], index: Int = 0): GlobalIndex[G] =
    GlobalIndex(Index[G](statement, index) :: indices)


  def leave_scope(): GlobalIndex[G] =
    GlobalIndex(indices.tail).make_step()


  def make_step(): GlobalIndex[G] = indices.head.make_step() match {
    case Some(index) => GlobalIndex(index :: indices.tail)
    case None => GlobalIndex(indices.tail).make_step()
  }

  def resolve(): Statement[G] = indices.head.resolve()
}

sealed trait Index[G] {
  def make_step(): Option[Index[G]]
  def resolve(): Statement[G]
}

object Index {
  def apply[G](pvl_branch: PVLBranch[G], index: Int): Index[G] = PVLBranchIndex(pvl_branch, index)
  def apply[G](pvl_loop: PVLLoop[G], index: Int): Index[G] = PVLLoopIndex(pvl_loop, index)
  def apply[G](label: Label[G], index: Int): Index[G] = LabelIndex(label)
  def apply[G](framed_proof: FramedProof[G], index: Int): Index[G] = FramedProofIndex(framed_proof)
  def apply[G](extract: Extract[G], index: Int): Index[G] = ExtractIndex(extract)
  def apply[G](eval: Eval[G], index: Int): Index[G] = EvalIndex(eval, index)
  def apply[G](invoke_procedure: InvokeProcedure[G], index: Int): Index[G] = InvokeProcedureIndex(invoke_procedure, index)
  def apply[G](invoke_constructor: InvokeConstructor[G], index: Int): Index[G] = InvokeConstructorIndex(invoke_constructor, index)
  def apply[G](invoke_method: InvokeMethod[G], index: Int): Index[G] = InvokeMethodIndex(invoke_method, index)
  def apply[G](block: Block[G], index: Int): Index[G] = BlockIndex(block, index)
  def apply[G](scope: Scope[G], index: Int): Index[G] = ScopeIndex(scope)
  def apply[G](branch: Branch[G], index: Int): Index[G] = BranchIndex(branch, index)
  def apply[G](indet_branch: IndetBranch[G], index: Int): Index[G] = IndetBranchIndex(indet_branch, index)
  def apply[G](switch: Switch[G], index: Int): Index[G] = SwitchIndex(switch)
  def apply[G](loop: Loop[G], index: Int): Index[G] = LoopIndex(loop, index)
  def apply[G](ranged_for: RangedFor[G], index: Int): Index[G] = RangedForIndex(ranged_for)
  def apply[G](try_catch_finally: TryCatchFinally[G], index: Int): Index[G] = TryCatchFinallyIndex(try_catch_finally, index)
  def apply[G](synchronized: Synchronized[G], index: Int): Index[G] = SynchronizedIndex(synchronized)
  def apply[G](par_invariant: ParInvariant[G], index: Int): Index[G] = ParInvariantIndex(par_invariant)
  def apply[G](par_atomic: ParAtomic[G], index: Int): Index[G] = ParAtomicIndex(par_atomic)
  def apply[G](par_barrier: ParBarrier[G], index: Int): Index[G] = ParBarrierIndex(par_barrier)
  def apply[G](vec_block: VecBlock[G], index: Int): Index[G] = VecBlockIndex(vec_block)
  def apply[G](wand_package: WandPackage[G], index: Int): Index[G] = WandPackageIndex(wand_package)
  def apply[G](model_do: ModelDo[G], index: Int): Index[G] = ModelDoIndex(model_do)
  def apply[G](cpp_lifetime_scope: CPPLifetimeScope[G], index: Int): Index[G] = CPPLifetimeScopeIndex(cpp_lifetime_scope)
  def apply[G](unresolved_seq_branch: UnresolvedSeqBranch[G], index: Int): Index[G] = UnresolvedSeqBranchIndex(unresolved_seq_branch, index)
  def apply[G](unresolved_seq_loop: UnresolvedSeqLoop[G], index: Int): Index[G] = UnresolvedSeqLoopIndex(unresolved_seq_loop, index)
  def apply[G](seq_branch: SeqBranch[G], index: Int): Index[G] = SeqBranchIndex(seq_branch, index)
  def apply[G](seq_loop: SeqLoop[G], index: Int): Index[G] = SeqLoopIndex(seq_loop)
  def apply[G](veymont_assign_expression: VeyMontAssignExpression[G], index: Int): Index[G] = VeyMontAssignExpressionIndex(veymont_assign_expression)
  def apply[G](communicatex: CommunicateX[G], index: Int): Index[G] = CommunicateXIndex(communicatex)
}

case class PVLBranchIndex[G](pvl_branch: PVLBranch[G], index: Int) extends Index[G] {
  override def make_step(): Option[Index[G]] = None
  override def resolve(): Statement[G] = pvl_branch.branches.apply(index)._2  // TODO: Handle expressions in branch conditions
}
case class PVLLoopIndex[G](pvl_loop: PVLLoop[G], index: Int) extends Index[G] {
  override def make_step(): Option[Index[G]] = {
    if (index < 3) Some(PVLLoopIndex(pvl_loop, index + 1))
    else None
  }
  override def resolve(): Statement[G] = index match {
    case 0 => pvl_loop.init
    case 1 => Eval(pvl_loop.cond)(pvl_loop.cond.o)
    case 2 => pvl_loop.body
    case 3 => pvl_loop.update
  }
}

case class LabelIndex[G](label: Label[G]) extends Index[G] {
  override def make_step(): Option[Index[G]] = None
  override def resolve(): Statement[G] = label.stat
}

case class FramedProofIndex[G](framed_proof: FramedProof[G]) extends Index[G] {
  override def make_step(): Option[Index[G]] = None
  override def resolve(): Statement[G] = framed_proof.body
}

case class ExtractIndex[G](extract: Extract[G]) extends Index[G] {
  override def make_step(): Option[Index[G]] = None
  override def resolve(): Statement[G] = extract.contractedStatement
}

case class EvalIndex[G](eval: Eval[G], index: Int) extends Index[G] {
  override def make_step(): Option[Index[G]] = None  // TODO: Implement expressions!
  override def resolve(): Statement[G] = ??? // TODO: Implement expressions!
}

case class InvokeProcedureIndex[G](invoke_procedure: InvokeProcedure[G], index: Int) extends Index[G] {
  override def make_step(): Option[Index[G]] = ???
  override def resolve(): Statement[G] = ???
}

case class InvokeConstructorIndex[G](invoke_constructor: InvokeConstructor[G], index: Int) extends Index[G] {
  override def make_step(): Option[Index[G]] = ???
  override def resolve(): Statement[G] = ???
}

case class InvokeMethodIndex[G](invoke_method: InvokeMethod[G], index: Int) extends Index[G] {
  override def make_step(): Option[Index[G]] = ???
  override def resolve(): Statement[G] = ???
}

case class BlockIndex[G](block: Block[G], index: Int) extends Index[G] {
  override def make_step(): Option[Index[G]] = {
    if (index < block.statements.size - 1) Some(BlockIndex(block, index + 1))
    else None
  }
  override def resolve(): Statement[G] = block.statements.apply(index)
}

case class ScopeIndex[G](scope: Scope[G]) extends Index[G] {
  override def make_step(): Option[Index[G]] = None
  override def resolve(): Statement[G] = scope.body
}

case class BranchIndex[G](branch: Branch[G], index: Int) extends Index[G] {
  override def make_step(): Option[Index[G]] = None
  override def resolve(): Statement[G] = branch.branches.apply(index)._2
}

case class IndetBranchIndex[G](indet_branch: IndetBranch[G], index: Int) extends Index[G] {
  override def make_step(): Option[Index[G]] = None
  override def resolve(): Statement[G] = indet_branch.branches.apply(index)
}

case class SwitchIndex[G](switch: Switch[G]) extends Index[G] {
  override def make_step(): Option[Index[G]] = None
  override def resolve(): Statement[G] = switch.body
}

case class LoopIndex[G](loop: Loop[G], index: Int) extends Index[G] {
  override def make_step(): Option[Index[G]] = {
    if (index < 3) Some(LoopIndex(loop, index + 1))
    else None
  }
  override def resolve(): Statement[G] = index match {
    case 0 => loop.init
    case 1 => Eval(loop.cond)(loop.cond.o)
    case 2 => loop.body
    case 3 => loop.update
  }
}

case class RangedForIndex[G](ranged_for: RangedFor[G]) extends Index[G] {
  override def make_step(): Option[Index[G]] = None
  override def resolve(): Statement[G] = ranged_for.body
}

case class TryCatchFinallyIndex[G](try_catch_finally: TryCatchFinally[G], index: Int) extends Index[G] {
  override def make_step(): Option[Index[G]] = {
    if (index == 0) Some(TryCatchFinallyIndex(try_catch_finally, 1))
    else None
  }
  override def resolve(): Statement[G] = index match {
    case 0 => try_catch_finally.body
    case 1 => try_catch_finally.after
  }
}

case class SynchronizedIndex[G](synchronized: Synchronized[G]) extends Index[G] {
  override def make_step(): Option[Index[G]] = None
  override def resolve(): Statement[G] = synchronized.body
}

case class ParInvariantIndex[G](par_invariant: ParInvariant[G]) extends Index[G] {
  override def make_step(): Option[Index[G]] = None
  override def resolve(): Statement[G] = par_invariant.content
}

case class ParAtomicIndex[G](par_atomic: ParAtomic[G]) extends Index[G] {
  override def make_step(): Option[Index[G]] = None
  override def resolve(): Statement[G] = par_atomic.content
}

case class ParBarrierIndex[G](par_barrier: ParBarrier[G]) extends Index[G] {
  override def make_step(): Option[Index[G]] = None
  override def resolve(): Statement[G] = par_barrier.content
}

case class VecBlockIndex[G](vec_block: VecBlock[G]) extends Index[G] {
  override def make_step(): Option[Index[G]] = None
  override def resolve(): Statement[G] = vec_block.content
}

case class WandPackageIndex[G](wand_package: WandPackage[G]) extends Index[G] {
  override def make_step(): Option[Index[G]] = None
  override def resolve(): Statement[G] = wand_package.proof
}

case class ModelDoIndex[G](model_do: ModelDo[G]) extends Index[G] {
  override def make_step(): Option[Index[G]] = None
  override def resolve(): Statement[G] = model_do.impl
}

case class CPPLifetimeScopeIndex[G](cpp_lifetime_scope: CPPLifetimeScope[G]) extends Index[G] {
  override def make_step(): Option[Index[G]] = None
  override def resolve(): Statement[G] = cpp_lifetime_scope.body
}

case class UnresolvedSeqBranchIndex[G](unresolved_seq_branch: UnresolvedSeqBranch[G], index: Int) extends Index[G] {
  override def make_step(): Option[Index[G]] = None
  override def resolve(): Statement[G] = unresolved_seq_branch.branches.apply(index)._2
}

case class UnresolvedSeqLoopIndex[G](unresolved_seq_loop: UnresolvedSeqLoop[G], index: Int) extends Index[G] {
  override def make_step(): Option[Index[G]] = {
    if (index == 0) Some(UnresolvedSeqLoopIndex(unresolved_seq_loop, 1))
    else None
  }
  override def resolve(): Statement[G] = index match {
    case 0 => Eval(unresolved_seq_loop.cond)(unresolved_seq_loop.cond.o)
    case 1 => unresolved_seq_loop.body
  }
}

case class SeqBranchIndex[G](seq_branch: SeqBranch[G], index: Int) extends Index[G] {
  override def make_step(): Option[Index[G]] = None
  override def resolve(): Statement[G] = index match {
    case 0 => seq_branch.yes
    case 1 => seq_branch.no.get
  }
}

case class SeqLoopIndex[G](seq_loop: SeqLoop[G]) extends Index[G] {
  override def make_step(): Option[Index[G]] = None
  override def resolve(): Statement[G] = seq_loop.body
}

case class VeyMontAssignExpressionIndex[G](veymont_assign_expression: VeyMontAssignExpression[G]) extends Index[G] {
  override def make_step(): Option[Index[G]] = None
  override def resolve(): Statement[G] = veymont_assign_expression.assign
}

case class CommunicateXIndex[G](communicatex: CommunicateX[G]) extends Index[G] {
  override def make_step(): Option[Index[G]] = None
  override def resolve(): Statement[G] = communicatex.assign
}