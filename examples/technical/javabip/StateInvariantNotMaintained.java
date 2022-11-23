package vct.examples.technical.javabip;

import org.javabip.annotations.*;
import org.javabip.api.PortType;

@Port(name = GO)
@ComponentType(initial = INIT, name = NAME)
@StatePredicate(state = "doneState", expr = "x >= 3")
public class StateInvariantNotMaintained {
    public static final String INIT = "initialState";
    public static final String DONE = "doneState";
    public static final String NAME = "oneComponentOneTransition";
    public static final String GO = "go";

    StateInvariantNotMaintained() { }

    private int x;

    @Transition(name = GO, source = INIT, target = DONE)
    public void goTransition() {
        x = 2;
    }
}

