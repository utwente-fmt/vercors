package vct.col.rewrite;

import hre.ast.MessageOrigin;
import vct.col.ast.stmt.composite.IfStatement;
import vct.col.ast.stmt.decl.Method.Kind;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.composite.BlockStatement;
import vct.col.ast.type.ASTReserved;
import vct.col.ast.type.PrimitiveSort;
import vct.col.ast.type.Type;
import vct.col.ast.stmt.decl.*;
import vct.col.ast.util.AbstractRewriter;

public class RandomizedIf extends AbstractRewriter {

  public RandomizedIf(ProgramUnit source) {
    super(source);
  }
  
  @Override
  public void visit(ASTClass cl){
    DeclarationStatement args[]=new DeclarationStatement[0];
    Method if_any_method=create.method_decl(create.primitive_type(PrimitiveSort.Boolean), null, "if_any_random", args, null);
    if_any_method.setFlag(ASTFlags.FINAL, true);

    if (if_any_method.getOrigin() == null) {
    	if_any_method.setOrigin(new MessageOrigin("Default origin"));
    }
    
    super.visit(cl);
    cl=(ASTClass)result;
    cl.add(if_any_method);
    result=cl;
  }

  @Override
  public void visit(Method m){
    if (m.kind==Kind.Plain && m.getBody()!=null){
      Type returns=rewrite(m.getReturnType());
      String name = m.name();
      Contract contract=rewrite(m.getContract());
      DeclarationStatement[] args=rewrite(m.getArgs());
      BlockStatement body=create.block(
          create.field_decl("if_any_bool",
          create.primitive_type(PrimitiveSort.Boolean))
      );
      BlockStatement tmp=currentBlock;
      currentBlock=body;
      if (m.getBody() instanceof BlockStatement){
        for(ASTNode s:(BlockStatement)m.getBody()){
          body.add(rewrite(s));
        }
      } else {
        body.addStatement(rewrite(m.getBody()));
      }
      currentBlock=tmp;
      result=create.method_decl(returns, contract, name, args, body);
    } else {
      super.visit(m);
    }
  }
  
  @Override
  public void visit(IfStatement s){
    int N=s.getCount();
    if (N==2 && s.getGuard(0).isReserved(ASTReserved.Any) && s.getGuard(1)==IfStatement.elseGuard()){
      IfStatement res=new IfStatement();
      currentBlock.addStatement(
          create.assignment(create.local_name("if_any_bool"),
          create.invokation(create.reserved_name(ASTReserved.This),null,"if_any_random"))
      );
      res.addClause(create.local_name("if_any_bool"), rewrite(s.getStatement(0)));
      res.addClause(IfStatement.elseGuard(), rewrite(s.getStatement(1)));
      res.setOrigin(s);
      result=res;
    } else {
      super.visit(s);
    }
  }

}
