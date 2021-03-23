package vct.parsers.rewrite;

import hre.ast.MessageOrigin;
import vct.col.ast.expr.KernelInvocation;
import vct.col.ast.expr.MethodInvokation;
import vct.col.ast.stmt.decl.ASTClass.ClassKind;
import vct.col.ast.stmt.decl.*;
import vct.col.ast.type.Type;
import vct.col.ast.type.TypeExpression;
import vct.col.ast.util.AbstractRewriter;

public class ConvertTypeExpressions extends AbstractRewriter {

  
  private ASTClass kernels;
  
  public ConvertTypeExpressions(ProgramUnit source) {
    super(source);
    create.enter();
    create.setOrigin(new MessageOrigin("Collected kernels"));
    kernels=create.ast_class("OpenCL",ClassKind.Kernel,null,null,null);
    create.leave();
  }
  
  private KernelBodyRewriter kbr=new KernelBodyRewriter(source());
  
  @Override
  public void visit(DeclarationStatement d){
    boolean extern=false;
    Type t=d.getType();
    while(t instanceof TypeExpression){
      TypeExpression e=(TypeExpression)t;
      switch (e.operator()) {
      case Static:
        t=e.firstType();
        break;
      case Extern:
        extern=true;
        t=e.firstType();
        break;        
      default:
        Fail("cannot deal with type operator %s", e.operator());
      }
    }
    DeclarationStatement res=create.field_decl(d.name(), rewrite(t), rewrite(d.initJava()));
    if (extern){
      res.setFlag(ASTFlags.EXTERN,true);
    }
    result=res;
  }
  
  @Override
  public void visit(Method m){
    super.visit(m);
    Method res = (Method)result;
    Type t=m.getReturnType();
    boolean kernel=false;
    while(t instanceof TypeExpression){
      TypeExpression e=(TypeExpression)t;
      switch (e.operator()) {
      case Static:
        res.setStatic(true);
        t=e.firstType();
        break;
      case Extern:
        res.setFlag(ASTFlags.EXTERN,true);
        t=e.firstType();
        break;        
      case Kernel:
        kernel=true;
        t=e.firstType();
        break;        
      default:
        Fail("cannot deal with type operator %s", e.operator());
      }
    }
    Debug("remaining type of %s is %s",m.getReturnType(),t);
    Method out=create.method_kind(
        res.getKind(),
        t,
        rewrite(res.getContract()),
        res.name(),
        rewrite(res.getArgs()),
        res.usesVarArgs(),
        rewrite(res.getBody()));
    out.copyMissingFlags(res);
    if(kernel){
      result=kbr.rewrite(out);
    } else {
      result=out;
    }
  }

  @Override
  public void visit(KernelInvocation ki) {
    MethodInvokation res = create.invokation(null, null, ki.method(), rewrite(ki.javaArgs()));

    res.get_before().addStatement(
            create.assignment(create.unresolved_name("opencl_gcount"), rewrite(ki.blockCount())));
    res.get_before().addStatement(
            create.assignment(create.unresolved_name("opencl_gsize"), rewrite(ki.threadCount())));

    result = res;
  }

  @Override
  public
  ProgramUnit rewriteAll(){
    ProgramUnit pu=super.rewriteAll();
    if (kernels.size()>0){
      pu.add(kernels);
    }
    return pu;
  }
}
