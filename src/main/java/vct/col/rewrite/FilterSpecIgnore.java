package vct.col.rewrite;

import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.composite.BlockStatement;
import vct.col.ast.stmt.decl.Method;
import vct.col.ast.stmt.decl.ProgramUnit;

import static vct.col.ast.stmt.decl.ASTSpecial.Kind.*;

public class FilterSpecIgnore extends AbstractRewriter {

  public FilterSpecIgnore(ProgramUnit source) {
    super(source);
  }
  
  @Override
  public void visit(BlockStatement block){
    BlockStatement res=create.block();
    int level=0;
    for(ASTNode S:block){
      if (level==0){
        if(S.isSpecial(SpecIgnoreEnd)){
          Fail("specification ignore end without begin");
        } else if (S.isSpecial(SpecIgnoreStart)){
          level++;
        } else {
          res.add(rewrite(S));
        }
      } else {
        if(S.isSpecial(SpecIgnoreEnd)){
          level--;
        } else if (S.isSpecial(SpecIgnoreStart)){
          level++;
        } 
      }
    }
    if (level!=0){
      Fail("specification ignore begin without end");
    }
    result=res;
  }

  @Override
  public void visit(Method m){
    vct.col.ast.stmt.decl.Contract c=m.getContract();
    if (c!=null && (c.pre_condition.isConstant(false) || c.pre_condition.isName("false"))){
      Warning("ignoring %s", m.name());
      result=null;
      return;
    }
    super.visit(m);
  }
}
