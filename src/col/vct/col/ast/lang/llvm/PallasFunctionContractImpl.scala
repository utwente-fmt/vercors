package vct.col.ast.lang.llvm

import vct.col.ast.PallasFunctionContract
import vct.col.ast.ops.PallasFunctionContractOps
import vct.col.print.{Ctx, Doc, DocUtil, Show, Text}

trait PallasFunctionContractImpl[G] extends PallasFunctionContractOps[G] {
  this: PallasFunctionContract[G] =>
  override def givenArgs = llvmGivenArgs.map(a => a.v)
  override def yieldsArgs = llvmYieldsArgs.map(a => a.v)

  override def layout(implicit ctx: Ctx): Doc =
    Doc.stack(Seq(
      Doc.stack(llvmGivenArgs.map(Text("given") <+> _.show <> ";")),
      Doc.stack(llvmYieldsArgs.map(Text("yields") <+> _.show <> ";")),
      DocUtil.clauses("requires", requires),
      DocUtil.clauses("ensures", ensures),
      Text("assumed") <+> assumed.toString <> ";",
      Text("external") <+> external.toString <> ";",
    ))

}
