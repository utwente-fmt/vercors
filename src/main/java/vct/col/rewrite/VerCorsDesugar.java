package vct.col.rewrite;

import java.util.ArrayList;
import java.util.concurrent.atomic.AtomicInteger;

import vct.col.ast.expr.OperatorExpression;
import vct.col.ast.expr.StandardOperator;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.type.PrimitiveSort;
import vct.col.ast.util.AbstractRewriter;

public class VerCorsDesugar extends AbstractRewriter {

  public VerCorsDesugar(ProgramUnit source) {
    super(source);
  }

  private AtomicInteger count=new AtomicInteger();
  
  @Override
  public void visit(OperatorExpression e){
    switch (e.operator()) {
    case Perm:{
      ArrayList<String> vars=new ArrayList<String>();
      ArrayList<ASTNode> range=new ArrayList<ASTNode>();
      ASTNode loc=desugar_loc(vars,range,e.arg(0));
      ASTNode frac=rewrite(e.arg(1));
      ASTNode res=create.expression(StandardOperator.Perm,loc,frac);
      int N=vars.size();
      for(int i=N-1;i>=0;i--){
        String name=vars.get(i);
        res=create.starall(
              create.expression(StandardOperator.Member,create.local_name(name),range.get(i)),
              res,
              create.field_decl(name, create.primitive_type(PrimitiveSort.Integer)));
      }
      result=res;
      break;
    }
    case LT:{
      ASTNode left=e.arg(0);
      if(left instanceof OperatorExpression){
        OperatorExpression lhs=(OperatorExpression)left;
        switch(lhs.operator()){
        case LTE:{
          ASTNode shared=rewrite(lhs.arg(1));
          lhs=rewrite(lhs);
          ASTNode rhs=rewrite(e.arg(1));
          rhs=create.expression(e.operator(),shared,rhs);
          result=create.expression(StandardOperator.And,lhs,rhs);
          break;
        }
        default:
          super.visit(e);
          break;
        }
      } else {
        super.visit(e);
      }
      break;
    }
    default:
      super.visit(e);
      break;
    }
  }

  private ASTNode desugar_loc(ArrayList<String> vars, ArrayList<ASTNode> range, ASTNode loc) {
    if (loc.isa(StandardOperator.Subscript)){
      OperatorExpression e=(OperatorExpression)loc;
      ASTNode base=desugar_loc(vars,range,e.arg(0));
      ASTNode idx=e.arg(1);
      if (idx.isa(StandardOperator.RangeSeq)){
        String iter="idx_"+count.incrementAndGet();
        vars.add(iter);
        range.add(rewrite(idx));
        idx=create.local_name(iter);
      } else {
        idx=rewrite(idx);
      }
      return create.expression(StandardOperator.Subscript, base,idx);
    } else {
      return rewrite(loc);
    }
  }
}
