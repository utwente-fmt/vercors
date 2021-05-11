package vct.col.ast

import vct.col.ast.expr.constant.BooleanValue
import vct.col.ast.RewriteHelpers._

class Rewriter extends AbstractRewriter {
  override def dispatch(stat: Statement): Statement = rewriteDefault(stat)
  override def dispatch(e: Expr): Expr = {
    e match {
      case node: BooleanValue =>
        new Constant.BooleanValue(node.value)(node.o)
    }
    rewriteDefault(e)
  }
  override def dispatch(t: Type): Type = rewriteDefault(t)
  override def dispatch(decl: Declaration): Unit = rewriteDefault(decl)

  override def dispatch(node: ApplicableContract): ApplicableContract = rewriteDefault(node)

  override def dispatch(parBlock: ParBlock): ParBlock = rewriteDefault(parBlock)
  override def dispatch(catchClause: CatchClause): CatchClause = rewriteDefault(catchClause)
  override def dispatch(fieldFlag: FieldFlag): FieldFlag = rewriteDefault(fieldFlag)
  override def dispatch(iterVariable: IterVariable): IterVariable = rewriteDefault(iterVariable)

  override def dispatch(silverPredAcc: SilverPredicateAccess): SilverPredicateAccess = rewriteDefault(silverPredAcc)

  override def dispatch(node: CDeclarator): CDeclarator = rewriteDefault(node)
  override def dispatch(cDeclSpec: CDeclarationSpecifier): CDeclarationSpecifier = rewriteDefault(cDeclSpec)
  override def dispatch(node: CTypeQualifier): CTypeQualifier = rewriteDefault(node)
  override def dispatch(node: CParam): CParam = rewriteDefault(node)
  override def dispatch(node: CPointer): CPointer = rewriteDefault(node)
  override def dispatch(node: CInit): CInit = rewriteDefault(node)
  override def dispatch(node: CDeclaration): CDeclaration = rewriteDefault(node)
}
