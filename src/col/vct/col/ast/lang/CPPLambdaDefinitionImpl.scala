package vct.col.ast.lang

import vct.col.ast.{CPPLambdaDefinition, CPPTLambda, Type}
import vct.col.print.{Ctx, Doc}

trait CPPLambdaDefinitionImpl[G] { this: CPPLambdaDefinition[G] =>
  override lazy val t: Type[G] = CPPTLambda[G]()

  override def layout(implicit ctx: Ctx): Doc =
    Doc.stack(Seq(
      contract, declarator.show <+> body.layoutAsBlock,
    ))
}