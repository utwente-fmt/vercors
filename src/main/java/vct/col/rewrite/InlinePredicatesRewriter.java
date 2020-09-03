package vct.col.rewrite;

import vct.col.ast.expr.NameExpression;
import vct.col.ast.stmt.decl.ASTFlags;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.decl.ASTSpecial;
import vct.col.ast.stmt.decl.Method;
import vct.col.ast.expr.MethodInvokation;
import vct.col.ast.expr.OperatorExpression;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.expr.StandardOperator;
import vct.col.ast.type.ASTReserved;
import vct.col.ast.type.PrimitiveSort;
import vct.col.ast.util.AbstractRewriter;

import java.util.HashMap;
import java.util.Stack;

public class InlinePredicatesRewriter extends AbstractRewriter {

  int count = 0;
  Stack<String> inlinedScalars = new Stack<>();
 
  public InlinePredicatesRewriter(ProgramUnit source) {
    super(source);
  }

  public ASTNode inline_call(MethodInvokation e, Method def) {
    int N=def.getArity();
    HashMap<NameExpression,ASTNode> map=new HashMap<NameExpression, ASTNode>();
    Substitution sigma=new Substitution(source(),map);
    map.put(create.reserved_name(ASTReserved.This), rewrite(e.object()));
    for(int i=0;i<N;i++){
      map.put(create.unresolved_name(def.getArgument(i)),rewrite(e.getArg(i)));
    }
    ASTNode body=rewrite(def.getBody());
    InlineMarking marker=new InlineMarking(source(),e.getOrigin());
    body.accept(marker);
    return sigma.rewrite(body);
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
        super.visit(e);
//        ASTNode arg1=rewrite(e.arg(0));
//        ASTNode arg2=rewrite(e.arg(1));
//        if (arg1 instanceof MethodInvokation || arg1.isa(StandardOperator.Scale)){
//          result=create.expression(StandardOperator.Unfolding,arg1,arg2);
//        } else {
//          Abort("Throwing away part of unfolding expression at %s because it is not a predicate or scale",
//                  arg1.getOrigin());
//          result=arg2;
//        }
        break;
      }
      case Scale:
        MethodInvokation scaledPredicate = (MethodInvokation) e.arg(1);
        if (inline(scaledPredicate.getDefinition())) {
          String scaleAmountName = "inlineScalar" + count++;

          inlinedScalars.push(scaleAmountName);
          super.visit(e);
          OperatorExpression scaleExpr = (OperatorExpression) result;
          inlinedScalars.pop();

          result = create.let_expr(
                  create.field_decl(scaleAmountName, rewrite(e.arg(0).getType()), scaleExpr.arg(0)),
                  scaleExpr.arg(1)
          );
        } else {
          super.visit(e);
        }
        break;
      case Perm:
        super.visit(e);
        OperatorExpression newPerm = (OperatorExpression) result;
        if (!inlinedScalars.empty()) {
          ASTNode permissionAmount = e.arg(1);
          for (String inlinedScalar : inlinedScalars) {
            permissionAmount = create.expression(
                    StandardOperator.Mult,
                    permissionAmount,
                    create.local_name(inlinedScalar));
          }
          result = create.expression(StandardOperator.Perm, newPerm.arg(0), permissionAmount);
        }
        break;
      default:
        super.visit(e);
        break;
    }
  }
  @Override
  public void visit(ASTSpecial e){
    super.visit(e);
//    switch(e.kind){
//      case Unfold:
//      case Fold:
//      {
//        ASTNode arg=rewrite(e.getArg(0));
////        if (arg instanceof MethodInvokation || arg.isa(StandardOperator.Scale)){
//          result=create.special(e.kind,arg);
//        /* } else {
//          Abort("Throwing away part of fold/unfold expression at %s because it is not a predicate or scale",
//                  e.getArg(0).getOrigin());
//          result=null; // returning null for a statement means already inserted or omit.
//        } */
//        break;
//      }
//      default:
//        super.visit(e);
//        break;
//    }
  }
}
