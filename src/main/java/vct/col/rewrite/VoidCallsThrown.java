package vct.col.rewrite;

import java.util.concurrent.atomic.AtomicInteger;

import vct.col.ast.stmt.composite.IfStatement;
import vct.col.ast.stmt.decl.ASTFlags;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.type.ASTReserved;
import vct.col.ast.stmt.decl.ASTSpecial;
import vct.col.ast.stmt.terminal.AssignmentStatement;
import vct.col.ast.stmt.composite.BlockStatement;
import vct.col.ast.stmt.composite.CatchClause;
import vct.col.ast.type.PrimitiveSort;
import vct.col.ast.util.ContractBuilder;
import vct.col.ast.stmt.decl.DeclarationStatement;
import vct.col.ast.stmt.decl.Method;
import vct.col.ast.expr.MethodInvokation;
import vct.col.ast.expr.NameExpression;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.stmt.terminal.ReturnStatement;
import vct.col.ast.expr.StandardOperator;
import vct.col.ast.stmt.composite.TryCatchBlock;
import vct.col.ast.stmt.decl.ASTSpecial.Kind;

public class VoidCallsThrown extends AbstractRewriter {

  public VoidCallsThrown(ProgramUnit source) {
    super(source);
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
    DeclarationStatement m_args[]=m.getArgs();
    int N=m_args.length;
    int K=m.getReturnType().isVoid()?1:2;
    DeclarationStatement args[]=new DeclarationStatement[N+K];
    args[0]=new DeclarationStatement("sys__thrown",create.class_type("Ref"),create.reserved_name(ASTReserved.Null));
    args[0].setOrigin(m);
    args[0].setFlag(ASTFlags.OUT_ARG, true);
    if (K==2){
      args[1]=new DeclarationStatement("sys__result",rewrite(m.getReturnType()));
      args[1].setOrigin(m);
      args[1].setFlag(ASTFlags.OUT_ARG, true);
    }
    for(int i=0;i<N;i++){
      args[i+K]=rewrite(m_args[i]);
    }
    ContractBuilder cb=new ContractBuilder();
    rewrite(m.getContract(),cb);
    result=create.method_decl(
      create.primitive_type(PrimitiveSort.Void),
      cb.getContract(),
      m.getName(),
      args,
      rewrite(m.getBody())
    );
  }
  
  private AtomicInteger count=new AtomicInteger();
  
  public void visit(TryCatchBlock tcb){
    int block_no=count.incrementAndGet();
    BlockStatement block=create.block();
    for (ASTNode S : tcb.main()){
      ASTNode rw=rewrite(S);
      block.add(rw);
      if (S.isSpecial(Kind.Throw) || rw instanceof MethodInvokation){
        block.add(create.ifthenelse(
          create.expression(StandardOperator.NEQ,create.local_name("sys__thrown"),create.reserved_name(ASTReserved.Null)),
          create.special(Kind.Goto, create.unresolved_name("Catches_"+block_no))
        ));
      }
    }
    block.add(create.special(Kind.Goto, create.unresolved_name("Finally_"+block_no)));
    block.add(create.special(Kind.Label,create.unresolved_name("Catches_"+block_no)));
    IfStatement catches=new IfStatement();
    catches.setOrigin(tcb.getOrigin());
    for (CatchClause c : tcb.catches()) {
      //ASTNode guard=create.expression(StandardOperator.Instance,create.local_name("sys__thrown"),rewrite(c.decl.getType()));
      ASTNode guard=create.expression(StandardOperator.EQ,
          create.invokation(null,null,"type_of",create.local_name("sys__thrown")),
          create.invokation(null,null,"class_Exception")
      );
      catches.addClause(guard,rewrite(c.block()));
    }
    catches.addClause(IfStatement.elseGuard(), create.special(Kind.Assert, create.constant(false)));
    block.add(catches);
    //block.add(create.special(Kind.Inhale,create.constant(false)));
    block.add(create.special(Kind.Label,create.unresolved_name("Finally_"+block_no)));
    if (tcb.after() != null) {
      for (ASTNode S : tcb.after()) {
        block.add(rewrite(S));
      }
    }
    result=block;
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
      if (current_method().getContract()!=null){
        res.add(create.special(ASTSpecial.Kind.Assert,rewrite(current_method().getContract().post_condition)));
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
    int N=e.getArity();
    ASTNode args[]=new ASTNode[N+1];
    args[0]=create.local_name("sys__thrown");
    for(int i=0;i<N;i++){
      args[i+1]=rewrite(e.getArg(i));
    }
    MethodInvokation res=create.invokation(rewrite(e.object), rewrite(e.dispatch) , e.method , args );
    for(NameExpression lbl:e.getLabels()){
      Debug("VOIDCALLS: copying label %s",lbl);
      res.addLabel(rewrite(lbl));
    }
    res.set_before(rewrite(e.get_before()));
    res.set_after(rewrite(e.get_after()));
    result=res;
  }
  
  public void visit(AssignmentStatement s){
    if (s.expression() instanceof MethodInvokation){
      MethodInvokation e=(MethodInvokation)s.expression();
      Method m=e.getDefinition();
      if (m==null) Abort("cannot process invokation of %s without definition",e.method);
      if (m.kind==Method.Kind.Plain){
        int N=e.getArity();
        ASTNode args[]=new ASTNode[N+2];
        args[0]=create.local_name("sys__thrown");
        args[1]=rewrite(s.location());
        for(int i=0;i<N;i++){
          args[i+2]=rewrite(e.getArg(i));
        }
        MethodInvokation res=create.invokation(rewrite(e.object), rewrite(e.dispatch) , e.method , args );
        for(NameExpression lbl:e.getLabels()){
          Debug("VOIDCALLS: copying label %s",lbl);
          res.addLabel(rewrite(lbl));
        }
        res.set_before(rewrite(e.get_before()));
        res.set_after(rewrite(e.get_after()));
        result=res;
        return;
      }
    }
    super.visit(s);
  }
  
  @Override
  public void visit(ASTSpecial s){
    switch(s.kind){
    case Throw:
      result=create.assignment(create.local_name("sys__thrown"),rewrite(s.args[0]));
      break;
    default:
      super.visit(s);
      break;
    }
  }
  
}
