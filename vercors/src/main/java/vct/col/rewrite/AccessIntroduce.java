package vct.col.rewrite;

import vct.col.ast.*;

public class AccessIntroduce extends AbstractRewriter {

  public AccessIntroduce(ProgramUnit source) {
    super(source);
  }

  @Override
  public void visit(Dereference e){
    //if (!in_ensures && !in_invariant && !in_requires){
      result = create.get_field(null, e.object(), e.field());
    //} else {
    //  super.visit(e);
    //}
  }
  
  @Override
  public void visit(OperatorExpression e){
    switch(e.operator()){
    case Assign:{
      ASTNode tmp=e.arg(0);
      if (tmp instanceof Dereference){
        Dereference loc=(Dereference)tmp;
        result = create.set_field(null, rewrite(loc.object()), loc.field(), rewrite(e.arg(1)));
      } else {
        super.visit(e);
      }
      break;
    }
    case Subscript:{
      super.visit(e);
      //result=create.expression(StandardOperator.Get,result);
      break;
    }
    default:
      super.visit(e);
      break;
    }
  }
  
  @Override
  public void visit(AssignmentStatement e){
    ASTNode tmp = e.location();
    if (tmp instanceof Dereference){
      Dereference loc=(Dereference)tmp;
      result = create.set_field(null, rewrite(loc.object()), loc.field(), rewrite(e.expression()));
    } else {
      super.visit(e);
    }
  }
}
