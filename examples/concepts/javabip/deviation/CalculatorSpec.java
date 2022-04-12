package org.javabip.spec.deviation;

import org.javabip.annotations.*;
import org.javabip.api.PortType;

import java.time.LocalDateTime;
import java.util.List;

import static org.javabip.spec.deviation.Constants.*;

// Ports({
         @Port(name = GET_DATA, type = PortType.enforceable)//,
         @Port(name = SEND_DATA, type = PortType.enforceable)//,
         @Port(name = START, type = PortType.enforceable)//,
         @Port(name = RESET, type = PortType.enforceable)
// })

@ComponentType(initial = INIT, name = CALCULATOR)
public class CalculatorSpec {

    private double mean;
    private double variance;

    @Transition(name = START, source = INIT, target = WORK)
    public void start() {
        // System.out.println("CALCULATOR: READY TO WORK");
        // System.out.println("start" + LocalDateTime.now());
    }

    // Transition(name = RESET, source = WORK, target = INIT)
    public void reset() {
        // System.out.println("CALCULATOR: TERMINATE");
    }


    @Transition(name = GET_DATA, source = WORK, target = CALCULATED, guard = "MEDIAN")
    public void work(@Data(name = INCOMING_DATA)List<Integer> data) {
        // System.out.println("CALCULATOR: CALCULATE DATA");

        // mean = data.stream().mapToInt(a -> a).average().orElse(0);
        // variance = data.stream().mapToDouble(a -> Math.pow(a - mean, 2)).sum();

        // System.out.println("MEAN: " + mean);
        // System.out.println("VARIANCE: " + variance);
    }

    @Transition(name = SEND_DATA, source = CALCULATED, target = WORK)
    public void send() throws InterruptedException {
        // System.out.println("CALCULATOR: DATA SENT");
        // Thread.sleep(1000);
    }

    @Data(name = OUTGOING_DATA_MEAN)
    public double getMean() {
        return mean;
    }

    @Data(name = OUTGOING_DATA_VARIANCE)
    public double getVariance() {
        return variance;
    }

    //@ requires data.get(0) == 3;
    @Guard(name = "MEDIAN")
    public boolean isSorted(@Data(name = INCOMING_DATA) List<Integer> data){
        //Collections.sort(data);
        return true;
    }
}

