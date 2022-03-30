package org.javabip.spec.deviation;

import akka.actor.ActorSystem;
import org.javabip.api.BIPEngine;
import org.javabip.api.BIPGlue;
import org.javabip.engine.factory.EngineFactory;
import org.javabip.glue.TwoSynchronGlueBuilder;

import static org.javabip.spec.deviation.Constants.*;

public class Main {
    public static void main(String[] args) throws Exception {
        ActorSystem system = ActorSystem.create(ACTOR_SYSTEM);
        EngineFactory engineFactory = new EngineFactory(system);

        BIPGlue glue = new TwoSynchronGlueBuilder() {
            @Override
            public void configure() {
                synchron(GeneratorSpec.class, SEND_DATA).to(CalculatorSpec.class, GET_DATA);
                synchron(CalculatorSpec.class, SEND_DATA).to(DeviatorSpec.class, QUERY_DATA);
                data(GeneratorSpec.class, OUTGOING_DATA).to(CalculatorSpec.class, INCOMING_DATA);
                data(CalculatorSpec.class, OUTGOING_DATA_MEAN).to(DeviatorSpec.class, INCOMING_DATA_MEAN);
                data(CalculatorSpec.class, OUTGOING_DATA_VARIANCE).to(DeviatorSpec.class, INCOMING_DATA_VARIANCE);
            }
        }.build();

        BIPEngine engine = engineFactory.create(ENGINE, glue);
        engine.register(new GeneratorSpec(), GENERATOR_SPEC, true);
        engine.register(new DeviatorSpec(), DEVIATOR_SPEC, true);
        engine.register(new CalculatorSpec(), CALCULATOR_SPEC, true);

        engine.start();
        engine.execute();

        //Thread.sleep(50000);
        while(true){
            Thread.sleep(60000);
            System.out.println("ping");
        }

        //engine.stop();
        //engineFactory.destroy(engine);
        //system.shutdown();
    }
}
