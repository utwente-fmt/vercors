package vct.col.rewrite;

import hre.lang.HREError;
import vct.col.ast.expr.StandardOperator;
import vct.col.ast.stmt.composite.BlockStatement;
import vct.col.ast.stmt.composite.IfStatement;
import vct.col.ast.stmt.decl.*;
import vct.col.ast.stmt.decl.ASTSpecial.Kind;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.terminal.ReturnStatement;
import vct.col.ast.type.Type;

/**
 * Transform function definitions written in statement syntax (pure methods)
 * into expression syntax.
 *  
 * @author Stefan Blom
 *
 */
public class PureMethodsAsFunctions extends AbstractRewriter {

  public PureMethodsAsFunctions(ProgramUnit source) {
    super(source);
  }
  
  @Override
  public void visit(Method m){
    if (m.kind==Method.Kind.Pure && (m.getBody() instanceof BlockStatement)){
      Type returns=rewrite(m.getReturnType());
      Contract contract=rewrite(m.getContract());
      DeclarationStatement args[]=rewrite(m.getArgs());
      ASTNode body=do_body((BlockStatement)m.getBody(),0);
      result=create.function_decl(returns, contract, m.name(), args, body);
    } else {
      super.visit(m);
    }
  }

  private ASTNode do_body(BlockStatement body, int i) {
    ASTNode S=body.get(i);
    while(S.isSpecial(Kind.Assert)){
      Warning("ignoring assert in pure method");
      i=i+1;
      S=body.get(i);
    }
    if (S instanceof BlockStatement){
      BlockStatement block=(BlockStatement)S;
      if (block.size()==2 && block.get(0).isSpecial(Kind.Assert)){
        S=block.get(1); 
      }
    }
    ASTNode res;
    create.enter();
    create.setOrigin(S.getOrigin());
    if (S instanceof ReturnStatement){
      ReturnStatement R=(ReturnStatement)S;
      res=rewrite(R.getExpression());
    } else if (S.isSpecial(Kind.Unfold)) {
      ASTSpecial e=(ASTSpecial)S;
      res=create.expression(StandardOperator.Unfolding,rewrite(e.getArg(0)),do_body(body,i+1));
    } else if (S instanceof IfStatement) {
      if (i!=body.getLength()-1){
        throw new HREError("if must be last statement in body");
      }
      IfStatement ITE=(IfStatement)S;
      if (ITE.getCount()!=2 || !ITE.getGuard(1).isConstant(true)){
        throw new HREError("unsupported variant of if-then %s",S);
      }
      res=create.expression(StandardOperator.ITE,
        ITE.getGuard(0),
        do_body((BlockStatement)ITE.getStatement(0),0),
        do_body((BlockStatement)ITE.getStatement(1),0)
      );
    } else {
      throw new HREError("unsupported pure statement %s",S);
    }
    create.leave();
    return res;
  }

}
