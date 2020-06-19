package vct.col.rewrite;

import vct.col.ast.expr.NameExpressionKind;
import vct.col.ast.util.ASTFrame;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.decl.DeclarationStatement;
import vct.col.ast.expr.NameExpression;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.util.AbstractRewriter;

public class RenamingRewriter extends AbstractRewriter {

  public RenamingRewriter(ProgramUnit source, ProgramUnit target) {
    super(source, target);
  }

  public RenamingRewriter(ProgramUnit source) {
    super(source);
  }

  public RenamingRewriter(ASTFrame<ASTNode> shared) {
    super(shared);
  }
  
  public String rename_method(String name){
    return name;
  }
  
  public String rename_variable(String name){
    return name;
  }

  public void visit(NameExpression e){
    if (e.getKind()== NameExpressionKind.Reserved){
      super.visit(e);
    } else {
      result = create.name(e.getKind(), null, rename_variable(e.getName()));
    }
  }
  
  public void visit(DeclarationStatement decl){
    result = create.field_decl(rename_variable(decl.name()), rewrite(decl.getType()));
  }
  
}
