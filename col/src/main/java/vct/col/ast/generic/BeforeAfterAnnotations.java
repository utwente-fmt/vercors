package vct.col.ast.generic;

import vct.col.ast.stmt.composite.BlockStatement;

public interface BeforeAfterAnnotations {

    public BlockStatement get_before();

    public BeforeAfterAnnotations set_before(BlockStatement block);

    public BlockStatement get_after();

    public BeforeAfterAnnotations set_after(BlockStatement block);

    default boolean hasBefore() {
        return get_before().size() > 0;
    }

    default boolean hasAfter() {
        return get_after().size() > 0;
    }
}
