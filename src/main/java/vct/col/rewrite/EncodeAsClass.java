package vct.col.rewrite;

import hre.ast.MessageOrigin;
import vct.col.ast.expr.MethodInvokation;
import vct.col.ast.expr.NameExpression;
import vct.col.ast.expr.NameExpressionKind;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.decl.ASTClass;
import vct.col.ast.stmt.decl.ASTDeclaration;
import vct.col.ast.stmt.decl.ASTFlags;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.util.AbstractRewriter;

public class EncodeAsClass extends AbstractRewriter {

  public EncodeAsClass(ProgramUnit source) {
    super(source);
    cl=new ASTClass("Ref",ASTClass.ClassKind.Plain);
    cl.setOrigin(new MessageOrigin("EncodeAsClass"));
    cl.setFlag(ASTFlags.FINAL,true);
  }

  private ASTClass cl;

  public ProgramUnit rewriteAll() {
    for(ASTDeclaration n:source().get()){
        ASTNode tmp=rewrite(n);
        if (tmp!=null){
          cl.add_dynamic(tmp);
        }
    }
    target().add(cl);
    return target();
  }

  public void visit(ASTClass cls) {
    super.visit(cls);
    target().add(result);
    result = null;
  }

  public void visit(NameExpression name) {
    if(name.kind() == NameExpressionKind.Local && name.site().isValidFlag(ASTFlags.STATIC) && name.site().isStatic()) {
      result = create.dereference(create.diz(), name.name());
    } else {
      super.visit(name);
    }
  }

  public void visit(MethodInvokation invok) {
    if(invok.object() == null) {
      MethodInvokation res = create.invokation(create.diz(), null, invok.method(), rewrite(invok.getArgs()));
      res.set_before(rewrite(invok.get_before()));
      res.set_after(rewrite(invok.get_after()));
      result = res;
    } else {
      super.visit(invok);
    }
  }
}
