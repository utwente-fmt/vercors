package vct.parsers.rewrite;

import hre.ast.MessageOrigin;
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

}
