package vct.examples.technical.javabip.transitionPrecondition;

import org.javabip.api.BIPEngine;
import org.javabip.api.BIPGlue;
import org.javabip.engine.factory.EngineFactory;
import org.javabip.glue.TwoSynchronGlueBuilder;

public class Main {
    public static void main(String[] args) {
        BIPGlue glue = new /*@ vercorsBipJob @*/ TwoSynchronGlueBuilder() {
            @Override
            public void configure() {
                port(MyComponent.class, "MyPort").requiresNothing();
                port(MyComponent.class, "MyPort").acceptsNothing();
            }
        }.build();
    }
}
