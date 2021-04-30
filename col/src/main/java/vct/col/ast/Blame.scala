package vct.col.ast

sealed trait ContractFailure
case class ContractFalse(node: Expr) extends ContractFailure
case class InsufficientPermissionToExhale(node: SilverResource) extends ContractFailure
case class ReceiverNotInjective(node: SilverResource) extends ContractFailure
case class NegativePermissionValue(node: SilverResource) extends ContractFailure

trait Scapegoat
  extends InternalErrorBlame
  with SilverAssignBlame
  with SilverUnfoldBlame
  with SilverFoldBlame
  with SilverWhileInvariantBlame
  with SilverInsufficientPermissionBlame
  with AssertBlame
  with ExhaleBlame
  with PreconditionBlame
  with PostconditionBlame
  with DivByZeroBlame
  with LabelNotReachedBlame
  with SeqBoundsBlame

trait InternalErrorBlame {
  def internalError(description: String): Unit
}

trait SilverAssignBlame {
  def silverAssignFailed(assign: SilverFieldAssign): Unit
}

trait AssertBlame {
  def assertFailed(failure: ContractFailure, assertion: Assert): Unit
}

trait ExhaleBlame {
  def exhaleFailed(failure: ContractFailure, exhale: Exhale): Unit
}

trait SilverUnfoldBlame {
  def silverUnfoldFailed(failure: ContractFailure, unfold: SilverUnfold): Unit
}

trait SilverFoldBlame {
  def silverFoldFailed(failure: ContractFailure, fold: SilverFold): Unit
}

trait PreconditionBlame {
  def preconditionFailed(failure: ContractFailure, invocation: Invocation): Unit
}

trait PostconditionBlame {
  def postconditionFailed(failure: ContractFailure, invokable: ContractApplicable)
}

trait SilverWhileInvariantBlame {
  def silverWhileInvariantNotEstablished(failure: ContractFailure, loop: SilverWhile): Unit
  def silverWhileInvariantNotMaintained(failure: ContractFailure, loop: SilverWhile): Unit
}

trait DivByZeroBlame {
  def divisionByZero(div: DividingExpr): Unit
}

trait SilverInsufficientPermissionBlame {
  def silverInsufficientPermission(deref: SilverDeref): Unit
}

trait LabelNotReachedBlame {
  def labelNotReached(old: Old): Unit
}

trait SeqBoundsBlame {
  def seqBoundNegative(subscript: SeqSubscript): Unit
  def seqBoundExceedsLength(subscript: SeqSubscript): Unit
}