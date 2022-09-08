package org.javabip.spec.deviation;

import org.javabip.annotations.*;
import org.javabip.api.PortType;

import java.time.LocalDateTime;
import java.util.List;

@ComponentType(initial = INIT, name = NAME)
@StatePredicate(state = "xyz", expr = "true")
public class GuardIsUsed {
    public static final String INIT = "initialState";
    public static final String NAME = "oneComponentOneTransition";
    public static final String GO = "go";
    public static final String GEQ_Y = "greaterEqualY";
    public static final String MY_INT = "myInt";

    private int x;

    @Transition(name = GO, source = INIT, target = INIT, guard = GEQ_Y)
    public void goTransition(@Data(name = MY_INT) int z) {
        //@ assert x >= z;
    }

    @Pure
    @Guard(name = GEQ_Y)
    public boolean geqZero(@Data(name = MY_INT) int y) {
        return x >= y;
    }
}

