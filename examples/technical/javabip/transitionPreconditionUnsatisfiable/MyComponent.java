package vct.examples.technical.javabip.transitionPreconditionUnsatisfiable;

import org.javabip.annotations.*;
import org.javabip.api.PortType;

@Port(name = "MyPort", type = PortType.enforceable)
@ComponentType(initial = "initialState", name = "myComponentSpec")
public class MyComponent {
    int x;

    /* If the precondition is an untruth, all aspects of the transition should be "not proven", as we cannot know
       in that case.
     */
    @Transition(name = "MyPort", source = "initialState", target = "initialState", pre = "42 == 1337")
    public void myUpdateFunction() {

    }
}
