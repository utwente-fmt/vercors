package vct.col.rewrite;

import vct.col.ast.stmt.decl.ASTFlags;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.decl.ASTSpecial;
import vct.col.ast.stmt.decl.Method;
import vct.col.ast.expr.MethodInvokation;
import vct.col.ast.expr.OperatorExpression;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.expr.StandardOperator;

public class InlinePredicatesRewriter extends AbstractRewriter {
 
  public InlinePredicatesRewriter(ProgramUnit source) {
    super(source);
  }
  
  @Override
  public void visit(MethodInvokation e){
    Method def=e.getDefinition();
    boolean inline;
    if (def==null){
      inline=false;
    } else {
      inline = inline(def);
    }
    if (inline){
      result=inline_call(e, def);
    } else {
      super.visit(e);
    }
  }

  protected boolean inline(Method def) {
    boolean inline;
    if (def.isValidFlag(ASTFlags.INLINE)){
      inline=(def.kind==Method.Kind.Predicate || def.kind==Method.Kind.Pure) && def.getFlag(ASTFlags.INLINE);
    } else {
      inline=false;
    }
    return inline;
  }

  @Override
  public void visit(Method m){
    if (inline(m)){
      result=null;
    } else {
      super.visit(m);
    }
  }
  
  @Override
  public void visit(OperatorExpression e){
    switch(e.operator()){
      case Unfolding:
      {
        ASTNode arg1=rewrite(e.arg(0));
        ASTNode arg2=rewrite(e.arg(1));
        if (arg1 instanceof MethodInvokation || arg1.isa(StandardOperator.Scale)){
          result=create.expression(StandardOperator.Unfolding,arg1,arg2);
        } else {
          result=arg2;
        }
        break;
      }
      default:
        super.visit(e);
        break;
    }
  }
  @Override
  public void visit(ASTSpecial e){
    switch(e.kind){
      case Unfold:
      case Fold:
      { 
        ASTNode arg=rewrite(e.getArg(0));
        if (arg instanceof MethodInvokation || arg.isa(StandardOperator.Scale)){
          result=create.special(e.kind,arg);
        } else {
          result=null; // returning null for a statement means already inserted or omit.
        }
        break;
      }
      default:
        super.visit(e);
        break;
    }
  }
}
