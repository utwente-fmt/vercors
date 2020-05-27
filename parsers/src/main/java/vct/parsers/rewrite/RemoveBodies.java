package vct.parsers.rewrite;

import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.composite.BlockStatement;
import vct.col.ast.stmt.decl.Contract;
import vct.col.ast.util.ContractBuilder;
import vct.col.ast.stmt.decl.DeclarationStatement;
import vct.col.ast.stmt.decl.Method;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.type.Type;
import vct.col.ast.util.AbstractRewriter;


public class RemoveBodies extends AbstractRewriter {

  public RemoveBodies(ProgramUnit source) {
    super(source);
  }
  
  @Override
  public void visit(Method m){
    String name=m.getName();
    if (currentContractBuilder==null) currentContractBuilder=new ContractBuilder();
    DeclarationStatement args[]=rewrite(m.getArgs());
    Contract mc=m.getContract();
    if (mc!=null){
      rewrite(mc,currentContractBuilder);
    }
    Method.Kind kind=m.kind;
    Type rt=rewrite(m.getReturnType());
    Contract c=currentContractBuilder.getContract();
    currentContractBuilder=null;
    ASTNode body;
    switch(kind){
    case Pure:
      if (m.getBody() instanceof BlockStatement){
        // The body of a pure method is hidden. 
        body=null;
      } else {
        // The body of a function is visible.
        body=rewrite(m.getBody());
      }
      break;
    case Predicate:
      // Bodies of predicates are visible.
      body=rewrite(m.getBody());
      break;
    default:
      // Everything else is hidden.
      body=null;
    }
    result=create.method_kind(kind, rt, c, name, args, m.usesVarArgs(), body);
  }

}
