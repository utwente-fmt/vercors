package vct.col.ast.util;

import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.decl.SignalsClause;
import vct.col.ast.stmt.decl.Contract;
import vct.col.ast.expr.OperatorExpression;
import vct.col.ast.expr.StandardOperator;
import vct.col.ast.syntax.Syntax;
import vct.col.ast.syntax.Syntax.Associativity;

public class Parenthesize extends AbstractRewriter {

  private final Syntax syntax;

  public Parenthesize(Syntax syntax){
    super(null,null);
    this.syntax=syntax;
  }

  public void rewrite(Contract c,ContractBuilder cb){
    if (c==null) return;
    cb.given(rewrite(c.given));
    cb.yields(rewrite(c.yields));
    if (c.modifies != null) cb.modifies(rewrite(c.modifies));
    if (c.accesses != null) cb.accesses(rewrite(c.accesses));
    cb.appendInvariant(rewrite(c.invariant));
    cb.requires(rewrite(c.pre_condition));
    cb.ensures(rewrite(c.post_condition));
    if (c.signals != null) {
      for (SignalsClause sc : c.signals) {
        cb.signals(sc.name(), rewrite(sc.type()), rewrite(sc.condition()));
      }
    }
  }

  @Override
  public void visit(OperatorExpression e){
    StandardOperator op=e.operator();
    ASTNode args[] = rewrite(e.argsJava()).toArray(new ASTNode[0]);
    if (syntax.isOperator(op)){
      for(int i=0;i<args.length;i++){
        if (args[i] instanceof OperatorExpression){
          StandardOperator child_op=((OperatorExpression)args[i]).operator();
          if (syntax.isOperator(child_op)){
            int prio=syntax.getPrecedence(child_op);
            if (i==0 && syntax.getAssociativity(op)==Associativity.Left
              ||i==(args.length-1) && syntax.getAssociativity(op)==Associativity.Right
            ){
              prio++;
            }
            if (prio<=syntax.getPrecedence(op)){
              args[i]=create.expression(StandardOperator.Wrap,args[i]);
            }
          }
        }
      }
    }
    result=create.expression(op,args);
  }
}
