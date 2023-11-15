package vct.parsers.transform.systemctocol.util;

import vct.col.origin.LabelContext;
import vct.col.origin.Origin;
import vct.col.origin.PreferredName;


public class OriGen {
    public static Origin create(String name) {
        return create().withContent(new PreferredName(Seqs.singleton(name)));
    }

    public static Origin create() {
        return new Origin(Seqs.singleton(new LabelContext("systemc")));
    }
}
