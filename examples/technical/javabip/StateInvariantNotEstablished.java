package vct.examples.technical.javabip;

import org.javabip.annotations.*;
import org.javabip.api.PortType;

@StatePredicate(state = INIT, expr = "x >= 3")
@ComponentType(initial = INIT, name = NAME)
public class StateInvariantNotEstablished {
    public static final String INIT = "initialState";
    public static final String NAME = "oneComponentOneTransition";

    OneComponentOneTransition() { }

    private int x;
}

