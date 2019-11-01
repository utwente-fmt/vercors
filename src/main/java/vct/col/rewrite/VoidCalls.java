package vct.col.rewrite;

import java.util.HashMap;

import vct.col.ast.stmt.decl.ASTFlags;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.type.ASTReserved;
import vct.col.ast.stmt.decl.ASTSpecial;
import vct.col.ast.stmt.terminal.AssignmentStatement;
import vct.col.ast.stmt.composite.BlockStatement;
import vct.col.ast.stmt.decl.DeclarationStatement;
import vct.col.ast.stmt.decl.Method;
import vct.col.ast.expr.MethodInvokation;
import vct.col.ast.expr.NameExpression;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.stmt.terminal.ReturnStatement;
import vct.col.ast.type.PrimitiveSort;
import vct.logging.ErrorMapping;
import vct.logging.VerCorsError.ErrorCode;

public class VoidCalls extends AbstractRewriter {

  private static final String RETURN_BRANCH = "return branch";

  public VoidCalls(ProgramUnit source, ErrorMapping map) {
    super(source);
    map.add(RETURN_BRANCH,ErrorCode.AssertFailed,ErrorCode.PostConditionFailed);
  }
  
  public void visit(NameExpression e){
    if (e.isReserved(ASTReserved.Result)){
      result=create.unresolved_name("sys__result");
    } else {
      super.visit(e);
    }
  }
  
  public void visit(Method m){
    switch(m.kind){
    case Predicate:
    case Pure:
      result=copy_rw.rewrite(m);
      return;
    default:
      break;
    }   
    if (m.getReturnType().isVoid()){
      super.visit(m);
    } else {
      DeclarationStatement m_args[]=m.getArgs();
      int N=m_args.length;
      DeclarationStatement args[]=new DeclarationStatement[N+1];
      args[0]=new DeclarationStatement("sys__result",rewrite(m.getReturnType()));
      args[0].setOrigin(m);
      args[0].setFlag(ASTFlags.OUT_ARG, true);
      for(int i=0;i<N;i++){
        args[i+1]=rewrite(m_args[i]);
      }
      result=create.method_decl(
          create.primitive_type(PrimitiveSort.Void),
          rewrite(m.getContract()),
          m.getName(),
          args,
          rewrite(m.getBody()));
    }
  }
  
  public void visit(ReturnStatement s){
    ASTNode expr=s.getExpression();
    BlockStatement res=create.block();
    if (expr!=null){
      res.add(create.assignment(create.local_name("sys__result"),rewrite(expr)));
    }
      for(ASTNode n : s.get_after()){
        res.add(rewrite(n));
      }
      ASTNode post=rewrite(current_method().getContract().post_condition);
      if (current_method().getContract()!=null){
        res.add(create.special(ASTSpecial.Kind.Assert,post).set_branch(RETURN_BRANCH));
      }
      res.add(create.special(ASTSpecial.Kind.Assume,create.constant(false)));
      result=res;
//    } else {
//      super.visit(s);
//    }
  }
  
  public void visit(MethodInvokation e){
    Method m=e.getDefinition();
    if (m==null) Abort("unexpected null method definition at %s",e.getOrigin());
    switch(m.kind){
    case Predicate:
    case Pure:
      super.visit(e);
      return;
    default:
      break;
    }
    if (!m.getReturnType().isVoid()){
      Fail("unexpected invokation of non-void method %s at %s",e.method,e.getOrigin());
    }
    super.visit(e);
  }
  
  public void visit(AssignmentStatement s){
    if (s.expression() instanceof MethodInvokation){
      MethodInvokation e=(MethodInvokation)s.expression();
      Method m=e.getDefinition();
      if (m==null) Abort("cannot process invokation of %s without definition",e.method);
      if (m.kind==Method.Kind.Plain){
        int N=e.getArity();
        ASTNode args[]=new ASTNode[N+1];
        args[0]=rewrite(s.location());
        for(int i=0;i<N;i++){
          args[i+1]=rewrite(e.getArg(i));
        }
        args[0]=rewrite(s.location());
        MethodInvokation res=create.invokation(rewrite(e.object), rewrite(e.dispatch) , e.method , args );
        for(NameExpression lbl:e.getLabels()){
          Debug("VOIDCALLS: copying label %s",lbl);
          res.addLabel(rewrite(lbl));
        }
        res.set_before(rewrite(e.get_before()));
        HashMap<NameExpression,ASTNode>map=new HashMap<NameExpression,ASTNode>();
        map.put(create.reserved_name(ASTReserved.Result),rewrite(s.location()));
        Substitution subst=new Substitution(source(),map);
        res.set_after(subst.rewrite(e.get_after()));
        result=res;
        return;
      }
    }
    super.visit(s);
  }
}
