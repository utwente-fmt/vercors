package org.javabip.spec.deviation;

import org.javabip.annotations.*;
import org.javabip.api.PortType;

import java.time.LocalDateTime;
import java.util.List;

@StatePredicate(state = INIT, expr = "x >= 3")
@ComponentType(initial = INIT, name = NAME)
public class StateInvariantNotMaintained {
    public static final String INIT = "initialState";
    public static final String NAME = "oneComponentOneTransition";

    OneComponentOneTransition() { }

    private int x;
}

