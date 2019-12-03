package vct.col.rewrite;

import vct.col.ast.generic.ASTNode;
import vct.col.ast.generic.BeforeAfterAnnotations;
import vct.col.ast.stmt.composite.BlockStatement;
import vct.col.ast.expr.MethodInvokation;
import vct.col.ast.stmt.decl.ProgramUnit;

public class FlattenBeforeAfter extends AbstractRewriter {

  public FlattenBeforeAfter(ProgramUnit source) {
    super(source);
  }

  
  @Override
  public void visit(BlockStatement s){
    BlockStatement res=create.block();
    for(ASTNode item:s){
      ASTNode tmp=rewrite(item);
      if ((tmp instanceof BeforeAfterAnnotations) && !(tmp instanceof MethodInvokation)){
        //Warning("moving %s",tmp.getClass());
        BeforeAfterAnnotations baa=(BeforeAfterAnnotations) tmp;
        BlockStatement before=baa.get_before();
        BlockStatement after=baa.get_after();
        baa.set_before(null);
        baa.set_after(null);
        
        if (before!=null) for(ASTNode n : before){
          n.clearParent();
          res.add(n);
        }
        res.add(tmp);
        if (after!=null) for(ASTNode n : after){
          n.clearParent();
          res.add(n);
        }
      } else {
        //if (tmp instanceof ASTSpecial){
        //  Warning("skipping %s containsing %s",((ASTSpecial)tmp).kind,((ASTSpecial)tmp).args[0].getClass());
        //}
        //Warning("skipping %s",tmp.getClass());
        res.add(tmp);
      }
    }
    result=res;
  }
}
