package vct.examples.technical.javabip;

import org.javabip.annotations.*;
import org.javabip.api.PortType;

@ComponentType(initial = INIT, name = NAME)
@StatePredicate(state = "xyz", expr = "true")
@Port(name = GO)
public class GuardIsUsed {
    public static final String INIT = "initialState";
    public static final String NAME = "oneComponentOneTransition";
    public static final String GO = "go";
    public static final String GEQ_ZERO = "greaterEqualZero";

    private int x;

    @Transition(name = GO, source = INIT, target = INIT, guard = GEQ_ZERO)
    public void goTransition() {
        //@ assert x >= 0;
    }

    @Pure
    @Guard(name = GEQ_ZERO)
    public boolean geqZero() {
        return x >= 0;
    }
}

