package vct.col.ast.generic;

import vct.col.ast.stmt.composite.BlockStatement;

public interface BeforeAfterAnnotations {

  public BeforeAfterAnnotations set_before(BlockStatement block);
  
  public BlockStatement get_before();
  
  public BeforeAfterAnnotations set_after(BlockStatement block);
  
  public BlockStatement get_after();

}
