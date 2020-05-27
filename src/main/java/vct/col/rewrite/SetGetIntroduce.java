package vct.col.rewrite;

import vct.col.ast.expr.Dereference;
import vct.col.ast.expr.OperatorExpression;
import vct.col.ast.expr.StandardOperator;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.stmt.terminal.AssignmentStatement;
import vct.col.ast.util.AbstractRewriter;

public class SetGetIntroduce extends AbstractRewriter {

  public SetGetIntroduce(ProgramUnit source) {
    super(source);
  }

  @Override
  public void visit(Dereference e){
    super.visit(e);
    if (!in_ensures && !in_invariant && !in_requires){
      result=create.expression(StandardOperator.Get,result);
    }
  }
  
  @Override
  public void visit(OperatorExpression e){
    switch(e.operator()){
    case Assign:{
      ASTNode tmp=rewrite(e.arg(0));
      if (tmp.isa(StandardOperator.Get)){
        tmp=((OperatorExpression)tmp).arg(0);
        result=create.expression(StandardOperator.Set,tmp,rewrite(e.arg(1)));
      } else {
        result=create.expression(StandardOperator.Assign,tmp,rewrite(e.arg(1)));
      }
      break;
    }
    case Subscript:{
      super.visit(e);
      result=create.expression(StandardOperator.Get,result);
      break;
    }
    default:
      super.visit(e);
      break;
    }
  }
  
  @Override
  public void visit(AssignmentStatement e){
    ASTNode tmp=rewrite(e.location());
    if (tmp.isa(StandardOperator.Get)){
      tmp=((OperatorExpression)tmp).arg(0);
      result=create.expression(StandardOperator.Set,tmp,rewrite(e.expression()));
    } else {
      result=create.expression(StandardOperator.Assign,tmp,rewrite(e.expression()));
    }    
  }
}
