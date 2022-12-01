package vct.examples.technical.javabip;

import org.javabip.annotations.*;
import org.javabip.api.PortType;

@Port(name = "MyPort", type = PortType.enforceable)
@ComponentType(initial = "initialState", name = "myComponentSpec")
public class MyComponent {
    // Player prepares a bet
    @Transition(name = "MyPort", source = "initialState", target = "initialState", pre = "1 > 2")
    public void myUpdateFunction() {

    }
}
