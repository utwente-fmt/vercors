package vct.parsers.transform.systemctocol.util;

import vct.col.origin.Origin;
import vct.col.origin.SourceNameOrigin;

public class OriGen {
    public static Origin create(String name) {
        return new SourceNameOrigin(name, new GeneratedOrigin());
    }

    public static Origin create() {
        return new GeneratedOrigin();
    }
}
