package vct.col.rewrite;

import vct.col.ast.stmt.decl.ASTSpecial;
import vct.col.ast.expr.OperatorExpression;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.expr.StandardOperator;

/**
 * This rewriter changes assignment expressions to assignment statements.
 * 
 * @author Stefan Blom
 */ 
public class AssignmentRewriter extends AbstractRewriter {

  public AssignmentRewriter(ProgramUnit source) {
    super(source);
  }

  @Override
  public void visit(ASTSpecial s){
    switch(s.kind){
    case Expression:
      if (s.args[0].isa(StandardOperator.Assign)){
        OperatorExpression e = (OperatorExpression)s.args[0];
        result = create.assignment(e.arg(0), e.arg(1));
        break;
      }
    default:
      super.visit(s);
      break;
    }
  }

}
