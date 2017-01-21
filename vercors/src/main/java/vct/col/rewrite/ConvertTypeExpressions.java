package vct.col.rewrite;

import hre.ast.MessageOrigin;
import vct.col.ast.*;
import vct.col.ast.ASTClass.ClassKind;

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
    DeclarationStatement res=create.field_decl(d.name,rewrite(t),rewrite(d.getInit()));
    if (extern){
      res.setFlag(ASTFlags.EXTERN,true);
    }
    result=res;
  }
  
  @Override
  public void visit(Method m){
    Method res=copy_rw.rewrite(m);
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
    System.err.printf("remaining type of %s is %s%n",m.getReturnType(),t);
    Method out=create.method_decl(
        t,
        copy_rw.rewrite(res.getContract()),
        res.name,
        copy_rw.rewrite(res.getArgs()),
        copy_rw.rewrite(res.getBody()));
    out.copyMissingFlags(res);
    if(kernel){
      result=kbr.rewrite(out);
    } else {
      result=out;
    }
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
