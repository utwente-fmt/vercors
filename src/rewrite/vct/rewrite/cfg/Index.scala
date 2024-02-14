package vct.rewrite.cfg

import vct.col.ast._

case class GlobalIndex[G](indices: List[Index[G]]) {

  def enter_scope(node: Node[G], index: Int = 0): GlobalIndex[G] =
    GlobalIndex(Index[G](node, index) :: indices)

  def make_step(): Set[GlobalIndex[G]] = {
    if (indices.isEmpty) return Set[GlobalIndex[G]]()
    val steps: Set[Option[Index[G]]] = indices.head.make_step()
    var res = Set[GlobalIndex[G]]()
    for (step <- steps) {
      step match {
        case Some(index) => res = res ++ GlobalIndex(index +: indices.tail)
        case None => res = res ++ GlobalIndex(indices.tail).make_step()
      }
    }
    res
  }

  def resolve(): Statement[G] = indices.head.resolve()

  def has_statement(): Boolean = indices.head.has_statement()

  def return_from_call(): GlobalIndex[G] = {
    // Find innermost subroutine call
    val stack: List[Index[G]] = indices.dropWhile {
      case InvokeProcedureIndex(_, _) | InvokeMethodIndex(_, _) => false
      case _ => true
    }
    // Find the next statement
    // TODO: Does this always return exactly one next step?
    GlobalIndex(stack.tail).make_step().head
  }

  def handle_exception(e: Expr[G]): GlobalIndex[G] = {
    // Find innermost try-catch block of appropriate type
    val stack: List[Index[G]] = indices.dropWhile {
      case TryCatchFinallyIndex(stmt, 0) => !stmt.catches.exists(c => c.decl.t.equals(e.t))
      case _ => true
    }
    // Unhandled exception
    if (stack.isEmpty) return GlobalIndex(stack)
    // Return to exception handler and go to catch block
    stack.head match {
      case TryCatchFinallyIndex(stmt, _) => GlobalIndex(TryCatchFinallyIndex(stmt, stmt.catches.indexWhere(c => c.decl.t.equals(e.t)) + 2) +: stack.tail)
    }
  }

  def handle_break(): GlobalIndex[G] = {
    // Find innermost occurrence of either a loop or a switch statement
    val stack: List[Index[G]] = indices.dropWhile {
      case PVLLoopIndex(_, 0) | LoopIndex(_, 0) => true    // If some godless heathen calls break in the init of a loop, don't consider that loop
      case PVLLoopIndex(_, _) | LoopIndex(_, _) | RangedForIndex(_) | UnresolvedSeqLoopIndex(_, _) | SeqLoopIndex(_) | SwitchIndex(_) => false
      case _ => true
    }
    // Find the next statement
    // TODO: Does this always return exactly one next step?
    GlobalIndex(stack.tail).make_step().head
  }

  def continue_innermost_loop(): GlobalIndex[G] = {
    // Find innermost loop that could be the target of continue
    val stack: List[Index[G]] = indices.dropWhile {
      case PVLLoopIndex(_, 0) | LoopIndex(_, 0) => true
      case PVLLoopIndex(_, _) | LoopIndex(_, _) | RangedForIndex(_) | UnresolvedSeqLoopIndex(_, _) | SeqLoopIndex(_) => false
      case _ => true
    }
    stack.head match {
      case PVLLoopIndex(pvl_loop, _) => GlobalIndex(PVLLoopIndex(pvl_loop, 1) +: stack.tail)
      case LoopIndex(loop, _) => GlobalIndex(LoopIndex(loop, 1) +: stack.tail)
      case UnresolvedSeqLoopIndex(unresolved_seq_loop, _) => GlobalIndex(UnresolvedSeqLoopIndex(unresolved_seq_loop, 0) +: stack.tail)
      case RangedForIndex(_) | SeqLoopIndex(_) => GlobalIndex(stack)
    }
  }
}

sealed trait Index[G] {
  /**
   * Defines the set of possible next steps. An index in the returned set indicates that this index can replace the
   * previous index at the top level of the index stack. A None value indicates that a step is possible, but it reaches
   * outside the scope of this index to the index below.
   *
   * @return A set of all steps possible from the current index
   */
  def make_step(): Set[Option[Index[G]]]

  /**
   * Returns the statement that corresponds to the current index.
   *
   * @return The statement at the current index
   */
  def resolve(): Statement[G]

  /**
   * Determines whether the index contains a statement.
   *
   * @return true if the index contains at least one statement, false otherwise
   */
  def has_statement(): Boolean = true
}

object Index {
  def apply[G](instance_method: InstanceMethod[G], index: Int): Index[G] = InitialIndex(instance_method)
  def apply[G](run_method: RunMethod[G], index: Int): Index[G] = RunMethodIndex(run_method)
  def apply[G](pvl_branch: PVLBranch[G], index: Int): Index[G] = PVLBranchIndex(pvl_branch, index)
  def apply[G](pvl_loop: PVLLoop[G], index: Int): Index[G] = PVLLoopIndex(pvl_loop, index)
  def apply[G](label: Label[G], index: Int): Index[G] = LabelIndex(label)
  def apply[G](framed_proof: FramedProof[G], index: Int): Index[G] = FramedProofIndex(framed_proof, index)
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
  def apply[G](assign: Assign[G], index: Int): Index[G] = AssignmentIndex(assign, index)
  def apply[G](statement: Statement[G], index: Int): Index[G] = ExpressionContainerIndex(statement, index)
}

case class InitialIndex[G](instance_method: InstanceMethod[G]) extends Index[G] {
  override def make_step(): Set[Option[Index[G]]] = Set(None)
  override def resolve(): Statement[G] = instance_method.body.get
}

case class RunMethodIndex[G](run_method: RunMethod[G]) extends Index[G] {
  override def make_step(): Set[Option[Index[G]]] = Set(None)
  override def resolve(): Statement[G] = run_method.body.get
}

case class ExpressionContainerIndex[G](statement: Statement[G], index: Int) extends Index[G] {
  override def make_step(): Set[Option[Index[G]]] = {
    if (index == 0) Set(Some(ExpressionContainerIndex(statement, 1)))
    else Set(None)
  }
  override def resolve(): Statement[G] = statement
}

case class AssignmentIndex[G](assign: Assign[G], index: Int) extends Index[G] {
  override def make_step(): Set[Option[Index[G]]] = {
    if (index < 2) Set(Some(AssignmentIndex(assign, index + 1)))
    else Set(None)
  }
  override def resolve(): Statement[G] = index match {
    case 0 => Eval(assign.target)(assign.target.o)
    case 1 => Eval(assign.value)(assign.value.o)
    case 2 => assign
  }
}

case class PVLBranchIndex[G](pvl_branch: PVLBranch[G], index: Int) extends Index[G] {
  override def make_step(): Set[Option[Index[G]]] = {
    // Indices 0, 2, 4, ... are the conditions, indices 1, 3, 5, ... are the branch bodies
    if (index % 2 == 0 && index < 2 * (pvl_branch.branches.size - 1))
      Set(Some(PVLBranchIndex(pvl_branch, index + 2)), Some(PVLBranchIndex(pvl_branch, index + 1)))
    else if (index == 2 * (pvl_branch.branches.size - 1))
      Set(Some(PVLBranchIndex(pvl_branch, index + 1)), None)
    else Set(None)
  }
  override def resolve(): Statement[G] = {
    if (index % 2 == 0) Eval(pvl_branch.branches.apply(index / 2)._1)(pvl_branch.branches.apply(index / 2)._1.o)
    else pvl_branch.branches.apply((index - 1) / 2)._2
  }
}

case class PVLLoopIndex[G](pvl_loop: PVLLoop[G], index: Int) extends Index[G] {
  override def make_step(): Set[Option[Index[G]]] = index match {
    case 0 => Set(Some(PVLLoopIndex(pvl_loop, 1)))
    case 1 => Set(Some(PVLLoopIndex(pvl_loop, 2)), None)
    case 2 => Set(Some(PVLLoopIndex(pvl_loop, 3)))
    case 3 => Set(Some(PVLLoopIndex(pvl_loop, 1)))
  }
  override def resolve(): Statement[G] = index match {
    case 0 => pvl_loop.init
    case 1 => Eval(pvl_loop.cond)(pvl_loop.cond.o)
    case 2 => pvl_loop.body
    case 3 => pvl_loop.update
  }
}

case class LabelIndex[G](label: Label[G]) extends Index[G] {
  override def make_step(): Set[Option[Index[G]]] = Set(None)
  override def resolve(): Statement[G] = label.stat
}

case class FramedProofIndex[G](framed_proof: FramedProof[G], index: Int) extends Index[G] {
  override def make_step(): Set[Option[Index[G]]] = {
    if (index < 2) Set(Some(FramedProofIndex(framed_proof, index + 1)))
    else Set(None)
  }
  override def resolve(): Statement[G] = index match {
    case 0 => Eval(framed_proof.pre)(framed_proof.pre.o)
    case 1 => Eval(framed_proof.post)(framed_proof.post.o)
    case 2 => framed_proof.body
  }
}

case class ExtractIndex[G](extract: Extract[G]) extends Index[G] {
  override def make_step(): Set[Option[Index[G]]] = Set(None)
  override def resolve(): Statement[G] = extract.contractedStatement
}

case class EvalIndex[G](eval: Eval[G], index: Int, subexpressions: Seq[Statement[G]]) extends Index[G] {
  def this(eval: Eval[G], index: Int) = this(eval, index, Utils.find_all_subexpressions(eval.expr))
  override def make_step(): Set[Option[Index[G]]] = {
    if (index < subexpressions.size - 1) Set(Some(EvalIndex(eval, index + 1)))
    else Set(None)
  }
  override def resolve(): Statement[G] = subexpressions.apply(index)
  override def has_statement(): Boolean = subexpressions.nonEmpty
}
object EvalIndex {
  def apply[G](eval: Eval[G], index: Int): EvalIndex[G] = new EvalIndex(eval, index)
}

case class InvokeProcedureIndex[G](invoke_procedure: InvokeProcedure[G], index: Int) extends Index[G] {
  override def make_step(): Set[Option[Index[G]]] = {
    if (index < invoke_procedure.args.size) Set(Some(InvokeProcedureIndex(invoke_procedure, index + 1)))
    else Set(None)
  }
  override def resolve(): Statement[G] = {
    if (index < invoke_procedure.args.size) Eval(invoke_procedure.args.apply(index))(invoke_procedure.args.apply(index).o)
    else invoke_procedure.ref.decl.body.get
  }
}

case class InvokeConstructorIndex[G](invoke_constructor: InvokeConstructor[G], index: Int) extends Index[G] {
  override def make_step(): Set[Option[Index[G]]] = {
    if (index < invoke_constructor.args.size) Set(Some(InvokeConstructorIndex(invoke_constructor, index + 1)))
    else Set(None)
  }
  override def resolve(): Statement[G] = {
    if (index < invoke_constructor.args.size) Eval(invoke_constructor.args.apply(index))(invoke_constructor.args.apply(index).o)
    else invoke_constructor.ref.decl.body.get
  }
}

case class InvokeMethodIndex[G](invoke_method: InvokeMethod[G], index: Int) extends Index[G] {
  override def make_step(): Set[Option[Index[G]]] = {
    if (index < invoke_method.args.size) Set(Some(InvokeMethodIndex(invoke_method, index + 1)))
    else Set(None)
  }
  override def resolve(): Statement[G] = {
    if (index < invoke_method.args.size) Eval(invoke_method.args.apply(index))(invoke_method.args.apply(index).o)
    else invoke_method.ref.decl.body.get
  }
}

case class BlockIndex[G](block: Block[G], index: Int) extends Index[G] {
  override def make_step(): Set[Option[Index[G]]] = {
    if (index < block.statements.size - 1) Set(Some(BlockIndex(block, index + 1)))
    else Set(None)
  }
  override def resolve(): Statement[G] = block.statements.apply(index)
}

case class ScopeIndex[G](scope: Scope[G]) extends Index[G] {
  override def make_step(): Set[Option[Index[G]]] = Set(None)
  override def resolve(): Statement[G] = scope.body
}

case class BranchIndex[G](branch: Branch[G], index: Int) extends Index[G] {
  override def make_step(): Set[Option[Index[G]]] = {
    // Indices 0, 2, 4, ... are the conditions, indices 1, 3, 5, ... are the branch bodies
    if (index % 2 == 0 && index < 2 * (branch.branches.size - 1))
      Set(Some(BranchIndex(branch, index + 2)), Some(BranchIndex(branch, index + 1)))
    else if (index == 2 * (branch.branches.size - 1))
      Set(Some(BranchIndex(branch, index + 1)), None)
    else Set(None)
  }
  override def resolve(): Statement[G] = {
    if (index % 2 == 0) Eval(branch.branches.apply(index / 2)._1)(branch.branches.apply(index / 2)._1.o)
    else branch.branches.apply((index - 1) / 2)._2
  }
}

case class IndetBranchIndex[G](indet_branch: IndetBranch[G], index: Int) extends Index[G] {
  override def make_step(): Set[Option[Index[G]]] = Set(None)
  override def resolve(): Statement[G] = indet_branch.branches.apply(index)
}

// TODO: Switch cases could be multiple context indices deep; this does not work with the single index for make_step()
case class SwitchIndex[G](switch: Switch[G]) extends Index[G] {
  override def make_step(): Set[Option[Index[G]]] = Set(None)
  override def resolve(): Statement[G] = switch.body
}

case class LoopIndex[G](loop: Loop[G], index: Int) extends Index[G] {
  override def make_step(): Set[Option[Index[G]]] = index match {
    case 0 => Set(Some(LoopIndex(loop, 1)))
    case 1 => Set(Some(LoopIndex(loop, 2)), None)
    case 2 => Set(Some(LoopIndex(loop, 3)))
    case 3 => Set(Some(LoopIndex(loop, 1)))
  }
  override def resolve(): Statement[G] = index match {
    case 0 => loop.init
    case 1 => Eval(loop.cond)(loop.cond.o)
    case 2 => loop.body
    case 3 => loop.update
  }
}

case class RangedForIndex[G](ranged_for: RangedFor[G]) extends Index[G] {
  override def make_step(): Set[Option[Index[G]]] = Set(None)
  override def resolve(): Statement[G] = ranged_for.body
}

case class TryCatchFinallyIndex[G](try_catch_finally: TryCatchFinally[G], index: Int) extends Index[G] {
  override def make_step(): Set[Option[Index[G]]] = {
    if (index != 1) Set(Some(TryCatchFinallyIndex(try_catch_finally, 1)))
    else Set(None)
  }
  override def resolve(): Statement[G] = index match {
    case 0 => try_catch_finally.body
    case 1 => try_catch_finally.after
    case _ => try_catch_finally.catches.apply(index - 2).body
  }
}

case class SynchronizedIndex[G](synchronized: Synchronized[G]) extends Index[G] {
  override def make_step(): Set[Option[Index[G]]] = Set(None)
  override def resolve(): Statement[G] = synchronized.body
}

case class ParInvariantIndex[G](par_invariant: ParInvariant[G]) extends Index[G] {
  override def make_step(): Set[Option[Index[G]]] = Set(None)
  override def resolve(): Statement[G] = par_invariant.content
}

case class ParAtomicIndex[G](par_atomic: ParAtomic[G]) extends Index[G] {
  override def make_step(): Set[Option[Index[G]]] = Set(None)
  override def resolve(): Statement[G] = par_atomic.content
}

case class ParBarrierIndex[G](par_barrier: ParBarrier[G]) extends Index[G] {
  override def make_step(): Set[Option[Index[G]]] = Set(None)
  override def resolve(): Statement[G] = par_barrier.content
}

case class VecBlockIndex[G](vec_block: VecBlock[G]) extends Index[G] {
  override def make_step(): Set[Option[Index[G]]] = Set(None)
  override def resolve(): Statement[G] = vec_block.content
}

case class WandPackageIndex[G](wand_package: WandPackage[G]) extends Index[G] {
  override def make_step(): Set[Option[Index[G]]] = Set(None)
  override def resolve(): Statement[G] = wand_package.proof
}

case class ModelDoIndex[G](model_do: ModelDo[G]) extends Index[G] {
  override def make_step(): Set[Option[Index[G]]] = Set(None)
  override def resolve(): Statement[G] = model_do.impl
}

case class CPPLifetimeScopeIndex[G](cpp_lifetime_scope: CPPLifetimeScope[G]) extends Index[G] {
  override def make_step(): Set[Option[Index[G]]] = Set(None)
  override def resolve(): Statement[G] = cpp_lifetime_scope.body
}

case class UnresolvedSeqBranchIndex[G](unresolved_seq_branch: UnresolvedSeqBranch[G], index: Int) extends Index[G] {
  override def make_step(): Set[Option[Index[G]]] = Set(None)
  override def resolve(): Statement[G] = unresolved_seq_branch.branches.apply(index)._2
}

case class UnresolvedSeqLoopIndex[G](unresolved_seq_loop: UnresolvedSeqLoop[G], index: Int) extends Index[G] {
  override def make_step(): Set[Option[Index[G]]] = index match {
    case 0 => Set(Some(UnresolvedSeqLoopIndex(unresolved_seq_loop, 1)), None)
    case 1 => Set(Some(UnresolvedSeqLoopIndex(unresolved_seq_loop, 0)))
  }
  override def resolve(): Statement[G] = index match {
    case 0 => Eval(unresolved_seq_loop.cond)(unresolved_seq_loop.cond.o)
    case 1 => unresolved_seq_loop.body
  }
}

case class SeqBranchIndex[G](seq_branch: SeqBranch[G], index: Int) extends Index[G] {
  override def make_step(): Set[Option[Index[G]]] = Set(None)
  override def resolve(): Statement[G] = index match {
    case 0 => seq_branch.yes
    case 1 => seq_branch.no.get
  }
}

case class SeqLoopIndex[G](seq_loop: SeqLoop[G]) extends Index[G] {
  override def make_step(): Set[Option[Index[G]]] = Set(None)
  override def resolve(): Statement[G] = seq_loop.body
}

case class VeyMontAssignExpressionIndex[G](veymont_assign_expression: VeyMontAssignExpression[G]) extends Index[G] {
  override def make_step(): Set[Option[Index[G]]] = Set(None)
  override def resolve(): Statement[G] = veymont_assign_expression.assign
}

case class CommunicateXIndex[G](communicatex: CommunicateX[G]) extends Index[G] {
  override def make_step(): Set[Option[Index[G]]] = Set(None)
  override def resolve(): Statement[G] = communicatex.assign
}