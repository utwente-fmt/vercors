package vct.col.ast.generic;

import vct.col.ast.stmt.composite.BlockStatement;

public interface BeforeAfterAnnotations {

  BeforeAfterAnnotations set_before(BlockStatement block);

  BlockStatement get_before();

  BeforeAfterAnnotations set_after(BlockStatement block);

  BlockStatement get_after();

  default boolean hasBefore() {
    return get_before().size() > 0;
  }

  default boolean hasAfter() {
    return get_after().size() > 0;
  }
}
