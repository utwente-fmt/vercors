package vct.examples.technical.javabip;

import org.javabip.annotations.*;
import org.javabip.api.PortType;

@ComponentType(initial = INIT, name = NAME)
@Invariant("x > 0")
public class ComponentInvariantNotEstablished {
    public static final String INIT = "initialState";
    public static final String NAME = "oneComponentOneTransition";

    OneComponentOneTransition() { }

    private int x;
}

