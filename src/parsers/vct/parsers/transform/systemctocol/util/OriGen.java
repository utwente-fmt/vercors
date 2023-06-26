package vct.parsers.transform.systemctocol.util;

import vct.col.ast.Blame1;
import vct.col.ast.BlameInput;
import vct.col.origin.Origin;
import vct.col.origin.SourceNameOrigin;

public class OriGen {
    public static Origin create(String name) {
        return new SourceNameOrigin(name, new GeneratedOrigin());
    }

    public static Origin create() {
        return new GeneratedOrigin();
    }

    public static <G> Blame1<G> createBlame() {
        return new Blame1(new BlameInput(create()), null/* FIXME PB */, create());
    }
}
