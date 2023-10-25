package vct.parsers.transform.systemctocol.util;

import vct.col.origin.Origin;


public class OriGen {
    public static Origin create(String name) {
        return new Origin(null);
    }

    public static Origin create() {
        return new GeneratedOrigin(null);
    }
}
