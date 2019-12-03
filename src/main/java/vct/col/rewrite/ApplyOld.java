package vct.col.rewrite;

import vct.col.ast.expr.OperatorExpression;
import vct.col.ast.expr.StandardOperator;

public class ApplyOld extends AbstractRewriter {

  private final AbstractRewriter rw_old;
  public ApplyOld(AbstractRewriter rw_old) {
    super(rw_old.source());
    this.rw_old=rw_old;
  }

  @Override
  public void visit(OperatorExpression oe){
    if (oe.isa(StandardOperator.Old)) {
      result = rw_old.rewrite(oe.first());
    } else {
      super.visit(oe);
    }
  }
}
