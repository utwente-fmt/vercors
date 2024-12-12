package vct.rewrite.cfg

import vct.col.ast._
import vct.col.ref.Ref

import scala.collection.mutable

sealed abstract class NextIndex[G] {
  def get: Index[G]
}
case class Step[G](value: Index[G]) extends NextIndex[G] {
  override def get: Index[G] = value
}
case class Outgoing[G]() extends NextIndex[G] {
  override def get: Index[G] =
    throw new NoSuchElementException("Trying to access outgoing edge")
}

case class GlobalIndex[G](indices: mutable.Seq[Index[G]]) {

  override def equals(obj: scala.Any): Boolean =
    obj match {
      case GlobalIndex(other_indices) =>
        indices.size == other_indices.size && indices.zip(other_indices)
          .forall(t => t._1.equals(t._2))
      case _ => false
    }

  def enter_scope(node: Node[G], index: Int = 0): GlobalIndex[G] =
    GlobalIndex(indices.prepended(Index[G](node, index)))

  def make_step(): mutable.Set[(GlobalIndex[G], Option[Expr[G]])] = {
    if (indices.isEmpty)
      return mutable.Set((this, None))
    val steps: Set[(NextIndex[G], Option[Expr[G]])] = indices.head.make_step()
    val res = mutable.Set[(GlobalIndex[G], Option[Expr[G]])]()
    for (step <- steps) {
      step match {
        case (Step(index), cond) =>
          res.addOne((GlobalIndex(index +: indices.tail), cond))
        case (Outgoing(), cond) =>
          res.addAll(GlobalIndex(indices.tail).make_step().map(tup =>
            (tup._1, Utils.and(tup._2, cond))
          ))
      }
    }
    res
  }

  def resolve(): Option[Statement[G]] =
    indices.dropWhile(i => !i.has_statement()).headOption match {
      case Some(idx) => Some(idx.resolve())
      case None => None
    }

  def has_statement(): Boolean =
    indices.nonEmpty && indices.head.has_statement()

  def return_from_call(): mutable.Set[(GlobalIndex[G], Option[Expr[G]])] = {
    // Find innermost subroutine call
    val stack: mutable.Seq[Index[G]] = indices.dropWhile {
      case InvokeProcedureIndex(_, _) | InvokeMethodIndex(_, _) => false
      case _ => true
    }
    // Find the possible next statements
    GlobalIndex(stack).make_step()
  }

  def handle_exception(e: Expr[G]): GlobalIndex[G] = {
    // Find innermost try-catch block of appropriate type
    val stack: mutable.Seq[Index[G]] = indices.dropWhile {
      case TryCatchFinallyIndex(stmt, 0) =>
        !stmt.catches.exists(c => c.decl.t.equals(e.t))
      case _ => true
    }
    // Unhandled exception
    if (stack.isEmpty)
      return GlobalIndex(stack)
    // Return to exception handler and go to catch block
    stack.head match {
      case TryCatchFinallyIndex(stmt, _) =>
        GlobalIndex(
          TryCatchFinallyIndex(
            stmt,
            stmt.catches.indexWhere(c => c.decl.t.equals(e.t)) + 2,
          ) +: stack.tail
        )
    }
  }

  def handle_break(): GlobalIndex[G] = {
    // Find innermost occurrence of either a loop or a switch statement
    val stack: mutable.Seq[Index[G]] = indices.dropWhile {
      case PVLLoopIndex(_, 0) | LoopIndex(_, 0) =>
        true // If some godless heathen calls break in the init of a loop, don't consider that loop
      case PVLLoopIndex(_, _) | LoopIndex(_, _) |
          RangedForIndex(_) | SwitchIndex(_) =>
        false
      case _ => true
    }
    // Find the next statement
    // TODO: Does this always return exactly one next step?
    GlobalIndex(stack.tail).make_step().head._1
  }

  def continue_innermost_loop(): GlobalIndex[G] = {
    // Find innermost loop that could be the target of continue
    val stack: mutable.Seq[Index[G]] = indices.dropWhile {
      case PVLLoopIndex(_, 0) | LoopIndex(_, 0) => true
      case PVLLoopIndex(_, _) | LoopIndex(_, _) | RangedForIndex(_) => false
      case _ => true
    }
    stack.head match {
      case PVLLoopIndex(pvl_loop, _) =>
        GlobalIndex(PVLLoopIndex(pvl_loop, 1) +: stack.tail)
      case LoopIndex(loop, _) => GlobalIndex(LoopIndex(loop, 1) +: stack.tail)
      case RangedForIndex(_) => GlobalIndex(stack)
    }
  }
}

sealed trait Index[G] {

  /** Defines the set of possible next steps. An index in the returned set
    * indicates that this index can replace the previous index at the top level
    * of the index stack. A None value indicates that a step is possible, but it
    * reaches outside the scope of this index to the index below. Along with
    * these steps, this method returns the conditions for such a step in the
    * form of a boolean expression, or None if a transition is certain to occur.
    *
    * @return
    *   A set of all steps possible from the current index, along with
    *   conditions for taking them
    */
  def make_step(): Set[(NextIndex[G], Option[Expr[G]])]

  /** Returns the statement that corresponds to the current index.
    *
    * @return
    *   The statement at the current index
    */
  def resolve(): Statement[G]

  /** Determines whether the index contains a statement.
    *
    * @return
    *   true if the index contains at least one statement, false otherwise
    */
  def has_statement(): Boolean = true
}

object Index {
  def from[G](node: Node[G], index: Int): Index[G] =
    node match {
      case procedure: Procedure[G] => InitialIndex(procedure)
      case run_method: RunMethod[G] => RunMethodIndex(run_method)
      case assign: Assign[G] => AssignmentIndex(assign, index)
      case pvl_branch: PVLBranch[G] => PVLBranchIndex(pvl_branch, index)
      case pvl_loop: PVLLoop[G] => PVLLoopIndex(pvl_loop, index)
      case label: Label[G] => LabelIndex(label)
      case framed_proof: FramedProof[G] => FramedProofIndex(framed_proof, index)
      case extract: Extract[G] => ExtractIndex(extract)
      case eval: Eval[G] => EvalIndex(eval, index)
      case invoke_procedure: InvokeProcedure[G] =>
        InvokeProcedureIndex(invoke_procedure, index)
      case invoke_constructor: InvokeConstructor[G] =>
        InvokeConstructorIndex(invoke_constructor, index)
      case invoke_method: InvokeMethod[G] =>
        InvokeMethodIndex(invoke_method, index)
      case block: Block[G] => BlockIndex(block, index)
      case scope: Scope[G] => ScopeIndex(scope)
      case branch: Branch[G] => BranchIndex(branch, index)
      case indet_branch: IndetBranch[G] => IndetBranchIndex(indet_branch, index)
      case switch: Switch[G] => SwitchIndex(switch)
      case loop: Loop[G] => LoopIndex(loop, index)
      case ranged_for: RangedFor[G] => RangedForIndex(ranged_for)
      case try_catch_finally: TryCatchFinally[G] =>
        TryCatchFinallyIndex(try_catch_finally, index)
      case synchronized: Synchronized[G] => SynchronizedIndex(synchronized)
      case par_invariant: ParInvariant[G] => ParInvariantIndex(par_invariant)
      case par_atomic: ParAtomic[G] => ParAtomicIndex(par_atomic)
      case par_barrier: ParBarrier[G] => ParBarrierIndex(par_barrier)
      case vec_block: VecBlock[G] => VecBlockIndex(vec_block)
      case wand_package: WandPackage[G] => WandPackageIndex(wand_package)
      case model_do: ModelDo[G] => ModelDoIndex(model_do)
      case cpp_lifetime_scope: CPPLifetimeScope[G] =>
        CPPLifetimeScopeIndex(cpp_lifetime_scope)
      case veymont_assign_expression: VeyMontAssignExpression[G] =>
        VeyMontAssignExpressionIndex(veymont_assign_expression)
      case communicatex: CommunicateX[G] => CommunicateXIndex(communicatex)
      case statement: ExpressionContainerStatement[G] =>
        ExpressionContainerIndex(statement, index)
    }

  def apply[G](node: Node[G], index: Int): Index[G] = from(node, index)
}

// TODO: Handle contract assertions for InitialIndex, RunMethodIndex, FramedProof, RangedForIndex, ???

case class InitialIndex[G](instance_method: Procedure[G]) extends Index[G] {
  override def make_step(): Set[(NextIndex[G], Option[Expr[G]])] =
    Set((Outgoing(), None))
  override def resolve(): Statement[G] = instance_method.body.get
  override def has_statement(): Boolean = instance_method.body.nonEmpty
  override def equals(obj: scala.Any): Boolean =
    obj match {
      case InitialIndex(m) => m.equals(instance_method)
      case _ => false
    }
}

case class RunMethodIndex[G](run_method: RunMethod[G]) extends Index[G] {
  override def make_step(): Set[(NextIndex[G], Option[Expr[G]])] =
    Set((Outgoing(), None))
  override def resolve(): Statement[G] = run_method.body.get
  override def has_statement(): Boolean = run_method.body.nonEmpty
  override def equals(obj: scala.Any): Boolean =
    obj match {
      case RunMethodIndex(m) => m.equals(run_method)
      case _ => false
    }
}

case class ExpressionContainerIndex[G](
    statement: ExpressionContainerStatement[G],
    index: Int,
) extends Index[G] {
  override def make_step(): Set[(NextIndex[G], Option[Expr[G]])] = {
    if (index == 0)
      Set((Step(ExpressionContainerIndex(statement, 1)), None))
    else
      Set((Outgoing(), None))
  }
  override def resolve(): Statement[G] =
    index match {
      case 0 => Eval(statement.expr)(statement.expr.o)
      case 1 => statement
    }
  override def equals(obj: scala.Any): Boolean =
    obj match {
      case ExpressionContainerIndex(s, i) => i == index && s.equals(statement)
      case _ => false
    }
}

case class AssignmentIndex[G](assign: Assign[G], index: Int) extends Index[G] {
  override def make_step(): Set[(NextIndex[G], Option[Expr[G]])] = {
    if (index < 2)
      Set((Step(AssignmentIndex(assign, index + 1)), None))
    else
      Set((Outgoing(), None))
  }
  override def resolve(): Statement[G] =
    index match {
      case 0 => Eval(assign.target)(assign.target.o)
      case 1 => Eval(assign.value)(assign.value.o)
      case 2 => assign
    }
  override def equals(obj: scala.Any): Boolean =
    obj match {
      case AssignmentIndex(a, i) => i == index && a.equals(assign)
      case _ => false
    }
}

case class PVLBranchIndex[G](pvl_branch: PVLBranch[G], index: Int)
    extends Index[G] {
  override def make_step(): Set[(NextIndex[G], Option[Expr[G]])] = {
    // Indices 0, 2, 4, ... are the conditions, indices 1, 3, 5, ... are the branch bodies
    if (index % 2 == 0 && index < 2 * (pvl_branch.branches.size - 1))
      Set(
        (
          Step(PVLBranchIndex(pvl_branch, index + 2)),
          Some(Utils.negate(pvl_branch.branches.apply(index / 2)._1)),
        ),
        (
          Step(PVLBranchIndex(pvl_branch, index + 1)),
          Some(pvl_branch.branches.apply(index / 2)._1),
        ),
      )
    else if (index == 2 * (pvl_branch.branches.size - 1))
      Set(
        (
          Step(PVLBranchIndex(pvl_branch, index + 1)),
          Some(pvl_branch.branches.apply(index / 2)._1),
        ),
        (
          Outgoing(),
          Some(Utils.negate(pvl_branch.branches.apply(index / 2)._1)),
        ),
      )
    else
      Set((Outgoing(), None))
  }
  override def resolve(): Statement[G] = {
    if (index % 2 == 0)
      Eval(pvl_branch.branches.apply(index / 2)._1)(
        pvl_branch.branches.apply(index / 2)._1.o
      )
    else
      pvl_branch.branches.apply((index - 1) / 2)._2
  }
  override def has_statement(): Boolean = pvl_branch.branches.nonEmpty
  override def equals(obj: scala.Any): Boolean =
    obj match {
      case PVLBranchIndex(b, i) => i == index && b.equals(pvl_branch)
      case _ => false
    }
}

case class PVLLoopIndex[G](pvl_loop: PVLLoop[G], index: Int) extends Index[G] {
  override def make_step(): Set[(NextIndex[G], Option[Expr[G]])] =
    index match {
      case 0 => Set((Step(PVLLoopIndex(pvl_loop, 1)), None))
      case 1 => Set((Step(PVLLoopIndex(pvl_loop, 2)), None))
      case 2 =>
        Set(
          (Step(PVLLoopIndex(pvl_loop, 3)), Some(pvl_loop.cond)),
          (Outgoing(), Some(Utils.negate(pvl_loop.cond))),
        )
      case 3 => Set((Step(PVLLoopIndex(pvl_loop, 4)), None))
      case 4 => Set((Step(PVLLoopIndex(pvl_loop, 1)), None))
    }
  override def resolve(): Statement[G] =
    index match {
      case 0 => pvl_loop.init
      case 1 =>
        Assert(Utils.loop_contract_to_expression(pvl_loop.contract))(
          pvl_loop.o
        )(pvl_loop.o)
      case 2 => Eval(pvl_loop.cond)(pvl_loop.cond.o)
      case 3 => pvl_loop.body
      case 4 => pvl_loop.update
    }
  override def equals(obj: scala.Any): Boolean =
    obj match {
      case PVLLoopIndex(l, i) => i == index && l.equals(pvl_loop)
      case _ => false
    }
}

case class LabelIndex[G](label: Label[G]) extends Index[G] {
  override def make_step(): Set[(NextIndex[G], Option[Expr[G]])] =
    Set((Outgoing(), None))
  override def resolve(): Statement[G] = label.stat
  override def equals(obj: scala.Any): Boolean =
    obj match {
      case LabelIndex(l) => l.equals(label)
      case _ => false
    }
}

case class FramedProofIndex[G](framed_proof: FramedProof[G], index: Int)
    extends Index[G] {
  override def make_step(): Set[(NextIndex[G], Option[Expr[G]])] = {
    if (index < 2)
      Set((Step(FramedProofIndex(framed_proof, index + 1)), None))
    else
      Set((Outgoing(), None))
  }
  override def resolve(): Statement[G] =
    index match {
      case 0 => Eval(framed_proof.pre)(framed_proof.pre.o)
      case 1 => Eval(framed_proof.post)(framed_proof.post.o)
      case 2 => framed_proof.body
    }
  override def equals(obj: scala.Any): Boolean =
    obj match {
      case FramedProofIndex(p, i) => i == index && p.equals(framed_proof)
      case _ => false
    }
}

case class ExtractIndex[G](extract: Extract[G]) extends Index[G] {
  override def make_step(): Set[(NextIndex[G], Option[Expr[G]])] =
    Set((Outgoing(), None))
  override def resolve(): Statement[G] = extract.contractedStatement
  override def equals(obj: scala.Any): Boolean =
    obj match {
      case ExtractIndex(e) => e.equals(extract)
      case _ => false
    }
}

case class EvalIndex[G](
    eval: Eval[G],
    index: Int,
    subexpressions: Seq[Statement[G]],
) extends Index[G] {
  def this(eval: Eval[G], index: Int) =
    this(eval, index, Utils.find_all_subexpressions(eval.expr))
  override def make_step(): Set[(NextIndex[G], Option[Expr[G]])] = {
    if (index < subexpressions.size - 1)
      Set((Step(EvalIndex(eval, index + 1)), None))
    else
      Set((Outgoing(), None))
  }
  override def resolve(): Statement[G] = subexpressions.apply(index)
  override def has_statement(): Boolean = subexpressions.nonEmpty
  override def equals(obj: scala.Any): Boolean =
    obj match {
      case EvalIndex(e, i, _) => i == index && e.equals(eval)
      case _ => false
    }
}
object EvalIndex {
  def apply[G](eval: Eval[G], index: Int): EvalIndex[G] =
    new EvalIndex(eval, index)
}

case class InvokeProcedureIndex[G](
    invoke_procedure: InvokeProcedure[G],
    index: Int,
) extends Index[G] {
  // Order of operations:
  // 1. args
  // 2. given
  // 3. outArgs
  // 4. yields
  // 5. precondition
  // 6. procedure body
  // 7. postcondition
  override def make_step(): Set[(NextIndex[G], Option[Expr[G]])] = {
    val args: Seq[Expr[G]] = invoke_procedure.args
    val givenMap: Seq[(Ref[G, Variable[G]], Expr[G])] =
      invoke_procedure.givenMap
    val outArgs: Seq[Expr[G]] = invoke_procedure.outArgs
    val yields: Seq[(Expr[G], Ref[G, Variable[G]])] = invoke_procedure.yields
    if (
      index < args.size + givenMap.size + outArgs.size + yields.size + 1 ||
      index == args.size + givenMap.size + outArgs.size + yields.size + 1 &&
      invoke_procedure.ref.decl.body.nonEmpty
    )
      Set((Step(InvokeProcedureIndex(invoke_procedure, index + 1)), None))
    else
      Set((Outgoing(), None))
  }
  override def resolve(): Statement[G] = {
    val args: Seq[Expr[G]] = invoke_procedure.args
    val givenMap: Seq[(Ref[G, Variable[G]], Expr[G])] =
      invoke_procedure.givenMap
    val outArgs: Seq[Expr[G]] = invoke_procedure.outArgs
    val yields: Seq[(Expr[G], Ref[G, Variable[G]])] = invoke_procedure.yields
    if (index < args.size) {
      val expr: Expr[G] = args.apply(index)
      Eval(expr)(expr.o)
    } else if (index < args.size + givenMap.size) {
      val expr: Expr[G] = givenMap.apply(index - args.size)._2
      Eval(expr)(expr.o)
    } else if (index < args.size + givenMap.size + outArgs.size) {
      val expr: Expr[G] = outArgs.apply(index - args.size - givenMap.size)
      Eval(expr)(expr.o)
    } else if (index < args.size + givenMap.size + outArgs.size + yields.size) {
      val expr: Expr[G] =
        yields.apply(index - args.size - givenMap.size - outArgs.size)._1
      Eval(expr)(expr.o)
    } else if (
      index == args.size + givenMap.size + outArgs.size + yields.size
    ) {
      val expr: Expr[G] =
        Star(
          Utils.contract_to_expression(
            invoke_procedure.ref.decl.contract.requires
          ),
          invoke_procedure.ref.decl.contract.contextEverywhere,
        )(invoke_procedure.o)
      Assert(expr)(expr.o)(expr.o)
    } else if (
      index == args.size + givenMap.size + outArgs.size + yields.size + 1 &&
      invoke_procedure.ref.decl.body.nonEmpty
    ) { invoke_procedure.ref.decl.body.get }
    else {
      val expr: Expr[G] =
        Star(
          Utils
            .contract_to_expression(invoke_procedure.ref.decl.contract.ensures),
          invoke_procedure.ref.decl.contract.contextEverywhere,
        )(invoke_procedure.o)
      Assert(expr)(expr.o)(expr.o)
    }
  }
  override def equals(obj: scala.Any): Boolean =
    obj match {
      case InvokeProcedureIndex(p, i) =>
        i == index && p.equals(invoke_procedure)
      case _ => false
    }
}

case class InvokeConstructorIndex[G](
    invoke_constructor: InvokeConstructor[G],
    index: Int,
) extends Index[G] {
  // Order of operations:
  // 1. args
  // 2. given
  // 3. outArgs
  // 4. yields
  // 5. out
  // 6. constructor body
  override def make_step(): Set[(NextIndex[G], Option[Expr[G]])] = {
    val args: Seq[Expr[G]] = invoke_constructor.args
    val givenMap: Seq[(Ref[G, Variable[G]], Expr[G])] =
      invoke_constructor.givenMap
    val outArgs: Seq[Expr[G]] = invoke_constructor.outArgs
    val yields: Seq[(Expr[G], Ref[G, Variable[G]])] = invoke_constructor.yields
    if (
      index < args.size + givenMap.size + outArgs.size + yields.size + 2 ||
      index == args.size + givenMap.size + outArgs.size + yields.size + 2 &&
      invoke_constructor.ref.decl.body.nonEmpty
    )
      Set((Step(InvokeConstructorIndex(invoke_constructor, index + 1)), None))
    else
      Set((Outgoing(), None))
  }
  override def resolve(): Statement[G] = {
    val args: Seq[Expr[G]] = invoke_constructor.args
    val givenMap: Seq[(Ref[G, Variable[G]], Expr[G])] =
      invoke_constructor.givenMap
    val outArgs: Seq[Expr[G]] = invoke_constructor.outArgs
    val yields: Seq[(Expr[G], Ref[G, Variable[G]])] = invoke_constructor.yields
    if (index < args.size) {
      val expr: Expr[G] = args.apply(index)
      Eval(expr)(expr.o)
    } else if (index < args.size + givenMap.size) {
      val expr: Expr[G] = givenMap.apply(index - args.size)._2
      Eval(expr)(expr.o)
    } else if (index < args.size + givenMap.size + outArgs.size) {
      val expr: Expr[G] = outArgs.apply(index - args.size - givenMap.size)
      Eval(expr)(expr.o)
    } else if (index < args.size + givenMap.size + outArgs.size + yields.size) {
      val expr: Expr[G] =
        yields.apply(index - args.size - givenMap.size - outArgs.size)._1
      Eval(expr)(expr.o)
    } else if (
      index == args.size + givenMap.size + outArgs.size + yields.size
    ) { Eval(invoke_constructor.out)(invoke_constructor.out.o) }
    else if (
      index == args.size + givenMap.size + outArgs.size + yields.size + 1
    ) {
      val expr: Expr[G] =
        Star(
          Utils.contract_to_expression(
            invoke_constructor.ref.decl.contract.requires
          ),
          invoke_constructor.ref.decl.contract.contextEverywhere,
        )(invoke_constructor.o)
      Assert(expr)(expr.o)(expr.o)
    } else if (
      index == args.size + givenMap.size + outArgs.size + yields.size + 2 &&
      invoke_constructor.ref.decl.body.nonEmpty
    ) { invoke_constructor.ref.decl.body.get }
    else {
      val expr: Expr[G] =
        Star(
          Utils.contract_to_expression(
            invoke_constructor.ref.decl.contract.ensures
          ),
          invoke_constructor.ref.decl.contract.contextEverywhere,
        )(invoke_constructor.o)
      Assert(expr)(expr.o)(expr.o)
    }
  }
  override def equals(obj: scala.Any): Boolean =
    obj match {
      case InvokeConstructorIndex(c, i) =>
        i == index && c.equals(invoke_constructor)
      case _ => false
    }
}

case class InvokeMethodIndex[G](invoke_method: InvokeMethod[G], index: Int)
    extends Index[G] {
  // Order of operations:
  // 1. obj
  // 2. args
  // 3. given
  // 4. outArgs
  // 5. yields
  // 6. method body
  override def make_step(): Set[(NextIndex[G], Option[Expr[G]])] = {
    val args: Seq[Expr[G]] = invoke_method.args
    val givenMap: Seq[(Ref[G, Variable[G]], Expr[G])] = invoke_method.givenMap
    val outArgs: Seq[Expr[G]] = invoke_method.outArgs
    val yields: Seq[(Expr[G], Ref[G, Variable[G]])] = invoke_method.yields
    if (
      index < args.size + givenMap.size + outArgs.size + yields.size + 2 ||
      index == args.size + givenMap.size + outArgs.size + yields.size + 2 &&
      invoke_method.ref.decl.body.nonEmpty
    )
      Set((Step(InvokeMethodIndex(invoke_method, index + 1)), None))
    else
      Set((Outgoing(), None))
  }
  override def resolve(): Statement[G] = {
    val args: Seq[Expr[G]] = invoke_method.args
    val givenMap: Seq[(Ref[G, Variable[G]], Expr[G])] = invoke_method.givenMap
    val outArgs: Seq[Expr[G]] = invoke_method.outArgs
    val yields: Seq[(Expr[G], Ref[G, Variable[G]])] = invoke_method.yields
    if (index == 0) { Eval(invoke_method.obj)(invoke_method.obj.o) }
    else if (index < args.size + 1) {
      val expr: Expr[G] = args.apply(index - 1)
      Eval(expr)(expr.o)
    } else if (index < args.size + givenMap.size + 1) {
      val expr: Expr[G] = givenMap.apply(index - args.size - 1)._2
      Eval(expr)(expr.o)
    } else if (index < args.size + givenMap.size + outArgs.size + 1) {
      val expr: Expr[G] = outArgs.apply(index - args.size - givenMap.size - 1)
      Eval(expr)(expr.o)
    } else if (
      index < args.size + givenMap.size + outArgs.size + yields.size + 1
    ) {
      val expr: Expr[G] =
        yields.apply(index - args.size - givenMap.size - outArgs.size - 1)._1
      Eval(expr)(expr.o)
    } else if (
      index == args.size + givenMap.size + outArgs.size + yields.size + 1
    ) {
      val expr: Expr[G] =
        Star(
          Utils
            .contract_to_expression(invoke_method.ref.decl.contract.requires),
          invoke_method.ref.decl.contract.contextEverywhere,
        )(invoke_method.o)
      Assert(expr)(expr.o)(expr.o)
    } else if (
      index == args.size + givenMap.size + outArgs.size + yields.size + 2 &&
      invoke_method.ref.decl.body.nonEmpty
    ) { invoke_method.ref.decl.body.get }
    else {
      val expr: Expr[G] =
        Star(
          Utils.contract_to_expression(invoke_method.ref.decl.contract.ensures),
          invoke_method.ref.decl.contract.contextEverywhere,
        )(invoke_method.o)
      Assert(expr)(expr.o)(expr.o)
    }
  }
  override def equals(obj: scala.Any): Boolean =
    obj match {
      case InvokeMethodIndex(m, i) => i == index && m.equals(invoke_method)
      case _ => false
    }
}

case class BlockIndex[G](block: Block[G], index: Int) extends Index[G] {
  override def make_step(): Set[(NextIndex[G], Option[Expr[G]])] = {
    if (index < block.statements.size - 1)
      Set((Step(BlockIndex(block, index + 1)), None))
    else
      Set((Outgoing(), None))
  }
  override def resolve(): Statement[G] = block.statements.apply(index)
  override def has_statement(): Boolean = block.statements.nonEmpty
  override def equals(obj: scala.Any): Boolean =
    obj match {
      case BlockIndex(b, i) => i == index && b.equals(block)
      case _ => false
    }
}

case class ScopeIndex[G](scope: Scope[G]) extends Index[G] {
  override def make_step(): Set[(NextIndex[G], Option[Expr[G]])] =
    Set((Outgoing(), None))
  override def resolve(): Statement[G] = scope.body
  override def equals(obj: scala.Any): Boolean =
    obj match {
      case ScopeIndex(s) => s.equals(scope)
      case _ => false
    }
}

case class BranchIndex[G](branch: Branch[G], index: Int) extends Index[G] {
  override def make_step(): Set[(NextIndex[G], Option[Expr[G]])] = {
    // Indices 0, 2, 4, ... are the conditions, indices 1, 3, 5, ... are the branch bodies
    if (index % 2 == 0 && index < 2 * (branch.branches.size - 1))
      Set(
        (
          Step(BranchIndex(branch, index + 2)),
          Some(Utils.negate(branch.branches.apply(index / 2)._1)),
        ),
        (
          Step(BranchIndex(branch, index + 1)),
          Some(branch.branches.apply(index / 2)._1),
        ),
      )
    else if (index == 2 * (branch.branches.size - 1))
      Set(
        (
          Step(BranchIndex(branch, index + 1)),
          Some(branch.branches.apply(index / 2)._1),
        ),
        (Outgoing(), Some(Utils.negate(branch.branches.apply(index / 2)._1))),
      )
    else
      Set((Outgoing(), None))
  }
  override def resolve(): Statement[G] = {
    if (index % 2 == 0)
      Eval(branch.branches.apply(index / 2)._1)(
        branch.branches.apply(index / 2)._1.o
      )
    else
      branch.branches.apply((index - 1) / 2)._2
  }
  override def has_statement(): Boolean = branch.branches.nonEmpty
  override def equals(obj: scala.Any): Boolean =
    obj match {
      case BranchIndex(b, i) => i == index && b.equals(branch)
      case _ => false
    }
}

case class IndetBranchIndex[G](indet_branch: IndetBranch[G], index: Int)
    extends Index[G] {
  override def make_step(): Set[(NextIndex[G], Option[Expr[G]])] =
    Set((Outgoing(), None))
  override def resolve(): Statement[G] = indet_branch.branches.apply(index)
  override def has_statement(): Boolean = indet_branch.branches.nonEmpty
  override def equals(obj: scala.Any): Boolean =
    obj match {
      case IndetBranchIndex(b, i) => i == index && b.equals(indet_branch)
      case _ => false
    }
}

case class SwitchIndex[G](switch: Switch[G]) extends Index[G] {
  override def make_step(): Set[(NextIndex[G], Option[Expr[G]])] =
    Set((Outgoing(), None))
  override def resolve(): Statement[G] = switch.body
  override def equals(obj: scala.Any): Boolean =
    obj match {
      case SwitchIndex(s) => s.equals(switch)
      case _ => false
    }
}

case class LoopIndex[G](loop: Loop[G], index: Int) extends Index[G] {
  override def make_step(): Set[(NextIndex[G], Option[Expr[G]])] =
    index match {
      case 0 => Set((Step(LoopIndex(loop, 1)), None))
      case 1 => Set((Step(LoopIndex(loop, 2)), None))
      case 2 =>
        Set(
          (Step(LoopIndex(loop, 3)), Some(loop.cond)),
          (Outgoing(), Some(Utils.negate(loop.cond))),
        )
      case 3 => Set((Step(LoopIndex(loop, 4)), None))
      case 4 => Set((Step(LoopIndex(loop, 1)), None))
    }
  override def resolve(): Statement[G] =
    index match {
      case 0 => loop.init
      case 1 =>
        Assert(Utils.loop_contract_to_expression(loop.contract))(loop.o)(loop.o)
      case 2 => Eval(loop.cond)(loop.cond.o)
      case 3 => loop.body
      case 4 => loop.update
    }
  override def equals(obj: scala.Any): Boolean =
    obj match {
      case LoopIndex(l, i) => i == index && l.equals(loop)
      case _ => false
    }
}

case class RangedForIndex[G](ranged_for: RangedFor[G]) extends Index[G] {
  override def make_step(): Set[(NextIndex[G], Option[Expr[G]])] =
    Set((Step(this), None), (Outgoing(), None))
  override def resolve(): Statement[G] = ranged_for.body
  override def equals(obj: scala.Any): Boolean =
    obj match {
      case RangedForIndex(r) => r.equals(ranged_for)
      case _ => false
    }
}

case class TryCatchFinallyIndex[G](
    try_catch_finally: TryCatchFinally[G],
    index: Int,
) extends Index[G] {
  override def make_step(): Set[(NextIndex[G], Option[Expr[G]])] = {
    if (index != 1)
      Set((Step(TryCatchFinallyIndex(try_catch_finally, 1)), None))
    else
      Set((Outgoing(), None))
  }
  override def resolve(): Statement[G] =
    index match {
      case 0 => try_catch_finally.body
      case 1 => try_catch_finally.after
      case _ => try_catch_finally.catches.apply(index - 2).body
    }
  override def equals(obj: scala.Any): Boolean =
    obj match {
      case TryCatchFinallyIndex(t, i) =>
        i == index && t.equals(try_catch_finally)
      case _ => false
    }
}

case class SynchronizedIndex[G](synchronized: Synchronized[G])
    extends Index[G] {
  override def make_step(): Set[(NextIndex[G], Option[Expr[G]])] =
    Set((Outgoing(), None))
  override def resolve(): Statement[G] = synchronized.body
  override def equals(obj: scala.Any): Boolean =
    obj match {
      case SynchronizedIndex(s) => s.equals(synchronized)
      case _ => false
    }
}

case class ParInvariantIndex[G](par_invariant: ParInvariant[G])
    extends Index[G] {
  override def make_step(): Set[(NextIndex[G], Option[Expr[G]])] =
    Set((Outgoing(), None))
  override def resolve(): Statement[G] = par_invariant.content
  override def equals(obj: scala.Any): Boolean =
    obj match {
      case ParInvariantIndex(p) => p.equals(par_invariant)
      case _ => false
    }
}

case class ParAtomicIndex[G](par_atomic: ParAtomic[G]) extends Index[G] {
  override def make_step(): Set[(NextIndex[G], Option[Expr[G]])] =
    Set((Outgoing(), None))
  override def resolve(): Statement[G] = par_atomic.content
  override def equals(obj: scala.Any): Boolean =
    obj match {
      case ParAtomicIndex(p) => p.equals(par_atomic)
      case _ => false
    }
}

case class ParBarrierIndex[G](par_barrier: ParBarrier[G]) extends Index[G] {
  override def make_step(): Set[(NextIndex[G], Option[Expr[G]])] =
    Set((Outgoing(), None))
  override def resolve(): Statement[G] = par_barrier.content
  override def equals(obj: scala.Any): Boolean =
    obj match {
      case ParBarrierIndex(p) => p.equals(par_barrier)
      case _ => false
    }
}

case class VecBlockIndex[G](vec_block: VecBlock[G]) extends Index[G] {
  override def make_step(): Set[(NextIndex[G], Option[Expr[G]])] =
    Set((Outgoing(), None))
  override def resolve(): Statement[G] = vec_block.content
  override def equals(obj: scala.Any): Boolean =
    obj match {
      case VecBlockIndex(v) => v.equals(vec_block)
      case _ => false
    }
}

case class WandPackageIndex[G](wand_package: WandPackage[G]) extends Index[G] {
  override def make_step(): Set[(NextIndex[G], Option[Expr[G]])] =
    Set((Outgoing(), None))
  override def resolve(): Statement[G] = wand_package.proof
  override def equals(obj: scala.Any): Boolean =
    obj match {
      case WandPackageIndex(w) => w.equals(wand_package)
      case _ => false
    }
}

case class ModelDoIndex[G](model_do: ModelDo[G]) extends Index[G] {
  override def make_step(): Set[(NextIndex[G], Option[Expr[G]])] =
    Set((Outgoing(), None))
  override def resolve(): Statement[G] = model_do.impl
  override def equals(obj: scala.Any): Boolean =
    obj match {
      case ModelDoIndex(m) => m.equals(model_do)
      case _ => false
    }
}

case class CPPLifetimeScopeIndex[G](cpp_lifetime_scope: CPPLifetimeScope[G])
    extends Index[G] {
  override def make_step(): Set[(NextIndex[G], Option[Expr[G]])] =
    Set((Outgoing(), None))
  override def resolve(): Statement[G] = cpp_lifetime_scope.body
  override def equals(obj: scala.Any): Boolean =
    obj match {
      case CPPLifetimeScopeIndex(c) => c.equals(cpp_lifetime_scope)
      case _ => false
    }
}

case class VeyMontAssignExpressionIndex[G](
    veymont_assign_expression: VeyMontAssignExpression[G]
) extends Index[G] {
  override def make_step(): Set[(NextIndex[G], Option[Expr[G]])] =
    Set((Outgoing(), None))
  override def resolve(): Statement[G] = veymont_assign_expression.assign
  override def equals(obj: scala.Any): Boolean =
    obj match {
      case VeyMontAssignExpressionIndex(v) =>
        v.equals(veymont_assign_expression)
      case _ => false
    }
}

case class CommunicateXIndex[G](communicatex: CommunicateX[G])
    extends Index[G] {
  override def make_step(): Set[(NextIndex[G], Option[Expr[G]])] =
    Set((Outgoing(), None))
  override def resolve(): Statement[G] = communicatex.assign
  override def equals(obj: scala.Any): Boolean =
    obj match {
      case CommunicateXIndex(c) => c.equals(communicatex)
      case _ => false
    }
}
