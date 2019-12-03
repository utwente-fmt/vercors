package vct.col.rewrite;

import hre.util.SingleNameSpace;
import vct.col.ast.expr.MethodInvokation;
import vct.col.ast.expr.StandardOperator;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.decl.*;
import vct.col.ast.type.ASTReserved;
import vct.col.ast.type.ClassType;
import vct.col.ast.type.PrimitiveSort;
import vct.col.ast.type.Type;
import vct.col.ast.util.ContractBuilder;

public class GenericPass1 extends AbstractRewriter {

  private SingleNameSpace<String, Type> map = new SingleNameSpace<String,Type>();
  private MultiSubstitution sigma = new MultiSubstitution(null,map);

  public GenericPass1(ProgramUnit source) {
    super(source);
  }
  
  @Override
  public void visit(ASTClass cl){
    map.enter();
    Contract c=cl.getContract();
    if (c==null){
      super.visit(cl);
      return;
    }
    for(DeclarationStatement decl:c.given){
      if (decl.getType().isPrimitive(PrimitiveSort.Class)){
        map.put(decl.name(), (Type)decl.initJava());
      }
    }
    super.visit(cl);
    map.leave();
  }
  
  @Override
  public void visit(ClassType t){
    result=sigma.rewrite(create.class_type(t.getFullName()));
  }
  
  @Override
  public void visit(MethodInvokation e){
    Type t=e.getType();
    Type rt=e.getDefinition().getReturnType();
    if(t.equals(rt) || (e.object instanceof Type)){
      super.visit(e);
    } else {
      Warning("invokation: %s != %s",t,rt);
      super.visit(e);
      ASTNode tmp=result;
      result=create.expression(StandardOperator.Cast,create.class_type(((ClassType)t).getFullName()),tmp);
    }
    //if (t instanceof ClassType && t.getArgCount()>0 && !(e.object instanceof Type)){
      //ASTNode tmp=create.expression(StandardOperator.Cast,create.class_type(((ClassType)t).getFullName()),rewrite(e.object));
      //result=create.invokation(tmp, e.dispatch, e.method, rewrite(e.getArgs()));
      
    //} else {
    //  super.visit(e);
    //}
  }
  
  @Override
  public void visit(Method m){
    map.enter();
    Type old_returns=m.getReturnType();
    Type returns=rewrite(old_returns);
    Warning("rewrite %s to %s %s",old_returns,returns,m.getName());
    ContractBuilder cb=new ContractBuilder();
    rewrite(m.getContract(),cb);
    if (old_returns.hasArguments() || !returns.equals(old_returns)){
        cb.ensures(create.expression(StandardOperator.Instance,
                create.reserved_name(ASTReserved.Result),
                copy_rw.rewrite(old_returns)
        ));     
    }
    
    /*
    if (returns instanceof ClassType && returns.getArgCount()>0){
      ASTClass cl=source().find((ClassType)returns);
      if (cl==null) Fail("undefined class: %s",returns);
      Contract c=cl.getContract();
      if (c==null) Fail("class without contract: %s",returns);
      int N=c.given.length;
      if (N!=returns.getArgCount()) Fail("argument count mismatch");
      for(int i=0;i<N;i++){
        cb.yields(create.field_decl("res_"+c.given[i].getName(),rewrite(c.given[i].getType())));
        cb.ensures(create.expression(StandardOperator.EQ,
            create.local_name("res_"+c.given[i].getName()),
            copy_rw.rewrite(returns.getArg(i))
        ));
            
      }
    }
    */
    String name=m.getName();
    DeclarationStatement old_args[]=m.getArgs();
    DeclarationStatement args[]=new DeclarationStatement[old_args.length];
    for(int i=0;i<old_args.length;i++){
      args[i]=rewrite(old_args[i]);
      if (old_args[i].getType().hasArguments() ||!args[i].getType().equals(old_args[i].getType())){
        cb.requires(create.expression(StandardOperator.Instance,
            create.local_name(old_args[i].name()),
            copy_rw.rewrite(old_args[i].getType())
    ));     
        
      }
    }
    ASTNode body=rewrite(m.getBody());
    result=create.method_decl(returns,cb.getContract(), name, args, body);
    map.leave();
  }

}
