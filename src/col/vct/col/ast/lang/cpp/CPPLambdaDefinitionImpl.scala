package vct.col.ast.lang.cpp

import vct.col.ast.{CPPLambdaDefinition, CPPTLambda, Type}
import vct.col.print.{Ctx, Doc}
import vct.col.ast.ops.CPPLambdaDefinitionOps

trait CPPLambdaDefinitionImpl[G] extends CPPLambdaDefinitionOps[G] { this: CPPLambdaDefinition[G] =>
  override lazy val t: Type[G] = CPPTLambda[G]()

  override def layout(implicit ctx: Ctx): Doc =
    Doc.stack(Seq(
      contract, declarator.show <+> body.layoutAsBlock,
    ))
}