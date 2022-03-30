package org.javabip.spec.deviation;

import org.javabip.annotations.*;
import org.javabip.api.PortType;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ThreadLocalRandom;

import static org.javabip.spec.deviation.Constants.*;
import static election.Constants.SENT;

@Ports({
        @Port(name = GENERATE, type = PortType.enforceable),
        @Port(name = SEND_DATA, type = PortType.enforceable),
        @Port(name = RESET, type = PortType.enforceable)
})

@ComponentType(initial = INIT, name = GENERATOR)
public class GeneratorSpec {
    private List<Integer> data = new ArrayList<>();
    //List<Integer> data = Collections.synchronizedList(d);
    private int min = 0;
    private int max = 1000;
    private int size = 100;

    @Transition(name = GENERATE, source = INIT, target = GENERATED)
    public void generate() {
        for (int i=0; i<size; i++){
            data.add(ThreadLocalRandom.current().nextInt(min, max + 1));
        }
        System.out.println("GENERATOR: DATA GENERATED");
    }

    @Transition(name = SEND_DATA, source = GENERATED, target = SENT)
    public void send() {
        System.out.println("GENERATOR: DATA SENT");
    }

    @Transition(name = RESET, source = SENT, target = INIT)
    public void reset() throws InterruptedException {
        //System.out.println("Inside reset" + data);
        data.clear();
    }


    @Data(name = OUTGOING_DATA)
    public List<Integer> getData() {
        return data;
    }
}


