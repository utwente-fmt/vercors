package vct.col.ast.family.contract

import vct.col.ast.{
  ApplicableContract,
  BooleanValue,
  Node,
  TResource,
  UnitAccountedPredicate,
}
import vct.col.ast.node.NodeFamilyImpl
import vct.col.check.{CheckContext, CheckError}
import vct.col.print._
import vct.col.ast.ops.{ApplicableContractFamilyOps, ApplicableContractOps}

trait ApplicableContractImpl[G]
    extends NodeFamilyImpl[G]
    with ApplicableContractOps[G]
    with ApplicableContractFamilyOps[G] {
  this: ApplicableContract[G] =>

  // Requires and ensures are checked in UnitAccountedPredicate
  override def check(context: CheckContext[G]): Seq[CheckError] =
    contextEverywhere.checkSubType(TResource()) ++
      kernelInvariant.checkSubType(TResource())

  override def checkContextRecursor[T](
      context: CheckContext[G],
      f: (CheckContext[G], Node[G]) => T,
  ): Seq[T] =
    this match {
      // Redundant match so this doesn't compile if we add a field to ApplicableContract
      case ApplicableContract(
            requires,
            ensures,
            contextEverywhere,
            kernelInvariant,
            signals,
            givenArgs,
            yieldsArgs,
            decreases,
          ) =>
        f(context.withUndeclared(yieldsArgs).withPrecondition, requires) +:
          f(context.withPostcondition, ensures) +: f(
            context.withUndeclared(yieldsArgs).withPrecondition,
            contextEverywhere,
          ) +: f(
            context.withUndeclared(yieldsArgs).withPrecondition,
            kernelInvariant,
          ) +:
          (signals.map(f(context, _)) ++ givenArgs.map(f(context, _)) ++
            yieldsArgs.map(f(context, _)) ++
            decreases.toSeq.map(f(context.withUndeclared(yieldsArgs), _)))
    }

  def isEmpty: Boolean =
    this match {
      case ApplicableContract(
            UnitAccountedPredicate(BooleanValue(true)),
            UnitAccountedPredicate(BooleanValue(true)),
            BooleanValue(true),
            BooleanValue(true),
            Nil,
            Nil,
            Nil,
            None,
          ) =>
        true
      case _ => false
    }

  def nonEmpty: Boolean = !isEmpty

  def layoutSpec(implicit ctx: Ctx): Doc =
    Doc.stack(Seq(
      Doc.stack(givenArgs.map(Text("given") <+> _.show <> ";")),
      Doc.stack(yieldsArgs.map(Text("yields") <+> _.show <> ";")),
      DocUtil.clauses("kernel_invariant", kernelInvariant),
      DocUtil.clauses("context_everywhere", contextEverywhere),
      DocUtil.clauses("requires", requires),
      Doc.stack(decreases.toSeq),
      DocUtil.clauses("ensures", ensures),
      Doc.stack(signals),
    ))

  def layoutSilver(implicit ctx: Ctx): Doc =
    Doc.stack(Seq(
      Doc.stack(givenArgs.map(Text("given") <+> _.show)),
      Doc.stack(yieldsArgs.map(Text("yields") <+> _.show)),
      DocUtil.clauses("kernel_invariant", kernelInvariant),
      DocUtil.clauses("context_everywhere", contextEverywhere),
      DocUtil.clauses("requires", requires),
      Doc.stack(decreases.toSeq),
      DocUtil.clauses("ensures", ensures),
      Doc.stack(signals),
    ))

  override def layout(implicit ctx: Ctx): Doc =
    ctx.syntax match {
      case Ctx.Silver => layoutSilver
      case _ => Doc.spec(Show.lazily(layoutSpec(_)))
    }
}
