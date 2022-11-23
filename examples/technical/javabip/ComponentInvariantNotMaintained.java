package vct.examples.technical.javabip;

import org.javabip.annotations.*;
import org.javabip.api.PortType;

@Port(name = GO)
@ComponentType(initial = INIT, name = NAME)
@Invariant("x >= 0")
public class ComponentInvariantNotMaintained {
    public static final String INIT = "initialState";
    public static final String DONE = "doneState";
    public static final String NAME = "oneComponentOneTransition";
    public static final String GO = "go";

    ComponentInvariantNotMaintained() {
        x = 0;
    }

    private int x;

    @Transition(name = GO, source = INIT, target = DONE, pre = "x >= 0", post = "x < 3")
    public void goTransition() {
        x = -1;
    }
}

