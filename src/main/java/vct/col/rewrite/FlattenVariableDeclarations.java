package vct.col.rewrite;

import vct.col.ast.stmt.decl.ASTFlags;
import vct.col.ast.stmt.decl.DeclarationStatement;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.stmt.decl.VariableDeclaration;

public class FlattenVariableDeclarations extends AbstractRewriter {

  public FlattenVariableDeclarations(ProgramUnit source) {
    super(source);
  }
  
  @Override
  public void visit(VariableDeclaration decl) {
    for(DeclarationStatement tmp:decl.flatten()){
      if (decl.isValidFlag(ASTFlags.STATIC)){
        tmp.setStatic(decl.isStatic());
      }
      current_sequence().add(tmp);
    }
    result=null;
  }

}
