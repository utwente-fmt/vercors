package org.javabip.spec.deviation;

import org.javabip.annotations.*;
import org.javabip.api.PortType;


/*[/expect bipComponentInvariantNotEstablished:false]*/
@ComponentType(initial = INIT, name = NAME)
@StatePredicate(state = "xyz", expr = "true")
@Invariant(expr = "false")
@Port(name = GO)
public class GuardIsUsed {
    public static final String INIT = "initialState";
    public static final String NAME = "oneComponentOneTransition";
    public static final String GO = "go";
    public static final String GEQ_Y = "greaterEqualY";

    GuardIsUsed() {

    }

    @Pure
    @Guard(name = GEQ_Y)
    /*[/expect bipGuardUnsatisfiablePrecondition:false]*/
    public boolean geqZero() {
        return true;
    }
    /*[/end]*/
}
/*[/end]*/
