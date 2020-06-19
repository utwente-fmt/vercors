package vct.col.rewrite;

import java.util.ArrayList;

import vct.col.ast.generic.ASTNode;
import vct.col.ast.expr.BindingExpression;
import vct.col.ast.expr.Binder;
import vct.col.ast.stmt.decl.DeclarationStatement;
import vct.col.ast.expr.OperatorExpression;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.expr.StandardOperator;
import vct.col.ast.util.AbstractRewriter;

public class OptimizeQuantifiers extends AbstractRewriter {

  public OptimizeQuantifiers(ProgramUnit source) {
    super(source);
  }
  
  private ASTNode strip(ArrayList<DeclarationStatement> decls,
      ArrayList<ASTNode> cond,BindingExpression e
  ){
    for(DeclarationStatement d:e.getDeclarations()){
      decls.add(rewrite(d));
    }
    cond.add(rewrite(e.select()));
    ASTNode main=e.main();
    while(main.isa(StandardOperator.Implies)){
      OperatorExpression oe=(OperatorExpression)main;
      cond.add(rewrite(oe.arg(0)));
      main=oe.arg(1);
    }
    if(main instanceof BindingExpression){
      BindingExpression tmp=(BindingExpression)main;
      if(tmp.binder()==e.binder() && (tmp.triggers()==null || tmp.javaTriggers().length==0)){
        enter(main);
        ASTNode temp=strip(decls,cond,tmp);
        leave(main);
        return temp;
      }
    }
    return rewrite(e.main());
  }
  
  public void visit(BindingExpression e){
    if ((e.binder()==Binder.Star || e.binder()==Binder.Forall) && (e.triggers()==null || e.javaTriggers().length==0)){
      ArrayList<DeclarationStatement> decls=new ArrayList<DeclarationStatement>();
      ArrayList<ASTNode> cond=new ArrayList<ASTNode>();
      ASTNode body=strip(decls,cond,e);
      result=create.binder(e.binder(), rewrite(e.result_type()),
          decls.toArray(new DeclarationStatement[0]),null,
          create.fold(StandardOperator.And,cond),body);
    } else {
      super.visit(e);
    }
  }

}
