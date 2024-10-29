package vct.col.ast.lang.c

import vct.col.ast.ops.{CDeclarationFamilyOps, CDeclarationOps}
import vct.col.ast.{CDeclaration, TResource}
import vct.col.check.{CheckContext, CheckError}
import vct.col.print._

trait CDeclarationImpl[G]
    extends CDeclarationOps[G] with CDeclarationFamilyOps[G] {
  this: CDeclaration[G] =>
  override def check(context: CheckContext[G]): Seq[CheckError] =
    kernelInvariant.checkSubType(TResource())

  // PB: Please keep in sync with ApplicableContractImpl
  def layoutContract(implicit ctx: Ctx): Doc =
    Doc.stack(Seq(
      Doc.stack(contract.givenArgs.map(Text("given") <+> _.show <> ";")),
      Doc.stack(contract.yieldsArgs.map(Text("yields") <+> _.show <> ";")),
      DocUtil.clauses("kernel_invariant", kernelInvariant),
      DocUtil.clauses("context_everywhere", contract.contextEverywhere),
      DocUtil.clauses("requires", contract.requires),
      Doc.stack(contract.decreases.toSeq),
      DocUtil.clauses("ensures", contract.ensures),
      Doc.stack(contract.signals),
    ))

  override def layout(implicit ctx: Ctx): Doc =
    Doc.stack(Seq(layoutContract, Group(Doc.spread(specs) <>> Doc.args(inits))))
}
