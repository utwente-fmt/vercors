package org.javabip.spec.deviation;

import org.javabip.annotations.*;
import org.javabip.api.PortType;

import java.time.LocalDateTime;
import java.util.List;

@ComponentType(initial = INIT, name = NAME)
@Invariant(expr = "x >= 0")
public class OneComponentOneTransition {
    public static final String INIT = "initialState";
    public static final String DONE = "doneState";
    public static final String NAME = "oneComponentOneTransition";
    public static final String GO = "go";

    OneComponentOneTransition() {
        x = 0;
    }

    private int x;

    @Transition(name = GO, source = INIT, target = DONE, requires = "x >= 0", ensures = "x >= 3")
    public void go() {
        x = 3;
    }
}

