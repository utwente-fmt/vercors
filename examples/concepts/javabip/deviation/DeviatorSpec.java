package org.javabip.spec.deviation;

import org.javabip.annotations.*;
import org.javabip.api.PortType;

import static org.javabip.spec.deviation.Constants.*;

import static java.lang.Math.sqrt;

/*
To verify:
-p
examples/concepts/javabip/deviation/ComponentInvariant.java
examples/concepts/javabip/deviation/DeviatorSpec.java
examples/concepts/javabip/deviation/GeneratorSpec.java
examples/concepts/javabip/deviation/Constants.java
--synchron
GeneratorSpec.SEND_DATA:CalculatorSpec.GET_DATA
--synchron
CalculatorSpec.SEND_DATA:CalculatorSpec.QUERY_DATA
--data
GeneratorSpec.OUTGOING_DATA:CalculatorSpec.INCOMING_DATA
--data
CalculatorSpec.OUTGOING_DATA_MEAN:DeviatorSpec.INCOMING_DATA_MEAN
--data
CalculatorSpec.OUTGOING_DATA_VARIANCE:DeviatorSpec.INCOMING_DATA_VARIANCE
 */

// Ports(
        @Port(name = QUERY_DATA)//, type = PortType.enforceable)
        @Port(name = CALCULATE_STD)//, type = PortType.enforceable)
// })



@ComponentType(initial = INIT, name = DEVIATOR)
public class DeviatorSpec {
    private double mean;
    private double variance;
    private double std;

    @Transition(name = QUERY_DATA, source = INIT, target = RCV)
    public void query(@Data(name = INCOMING_DATA_MEAN) double variance, @Data(name = INCOMING_DATA_VARIANCE) double mean) {
        // System.out.println("DEVIATOR: RECEIVED DATA");
        this.mean = mean;
        this.variance = variance;
    }

    @Transition(name = CALCULATE_STD, source = RCV, target = INIT)
    public void deviate() {
        std = sqrt(variance);
        // System.out.println("STD: " + std);
        // System.out.println("DEVIATOR: TERMINATE");
    }
}
