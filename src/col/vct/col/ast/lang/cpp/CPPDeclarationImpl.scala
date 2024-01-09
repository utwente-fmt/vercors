package vct.col.ast.lang.cpp

import vct.col.ast.CPPDeclaration
import vct.col.print._
import vct.col.ast.ops.{CPPDeclarationOps, CPPDeclarationFamilyOps}

trait CPPDeclarationImpl[G] extends CPPDeclarationOps[G] with CPPDeclarationFamilyOps[G] { this: CPPDeclaration[G] =>
  // PB: Please keep in sync with ApplicableContractImpl
  def layoutContract(implicit ctx: Ctx): Doc =
    Doc.stack(Seq(
      Doc.stack(contract.givenArgs.map(Text("given") <+> _.show <> ";")),
      Doc.stack(contract.yieldsArgs.map(Text("yields") <+> _.show <> ";")),
      DocUtil.clauses("context_everywhere", contract.contextEverywhere),
      DocUtil.clauses("requires", contract.requires),
      Doc.stack(contract.decreases.toSeq),
      DocUtil.clauses("ensures", contract.ensures),
      Doc.stack(contract.signals),
    ))

  override def layout(implicit ctx: Ctx): Doc =
    Doc.stack(Seq(
      layoutContract,
      Group(
        Doc.spread(specs) <>> Doc.args(inits)
      ),
    ))
}