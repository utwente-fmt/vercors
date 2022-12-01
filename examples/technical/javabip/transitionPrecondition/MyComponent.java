package vct.examples.technical.javabip.transitionPrecondition;

import org.javabip.annotations.*;
import org.javabip.api.PortType;

@Port(name = "MyPort", type = PortType.enforceable)
@ComponentType(initial = "initialState", name = "myComponentSpec")
public class MyComponent {
    int x;

    /* Tricky precondition: it is not satisfied in the synchronization, however,
       it does not invalidate the postcondition, state, or component invariant. So those should all verify,
       and be indicated as such in the verification report.
     */
    @Transition(name = "MyPort", source = "initialState", target = "initialState", pre = "x == 0")
    public void myUpdateFunction() {

    }
}
