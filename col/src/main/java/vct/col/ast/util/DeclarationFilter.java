package vct.col.ast.util;

import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.decl.DeclarationStatement;
import hre.util.Function;

public class DeclarationFilter implements Function<ASTNode,DeclarationStatement> {

  @Override
  public DeclarationStatement apply(ASTNode e) {
    if(e instanceof DeclarationStatement){
      return (DeclarationStatement)e;
    } else {
      return null;
    }
  }
}
