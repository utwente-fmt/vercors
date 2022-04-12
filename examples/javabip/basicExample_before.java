/**
 * Main.java
 */

public class Main {
    public static void main(String[] args) {
        ActorSystem system = ActorSystem.create(ACTOR_SYSTEM);
        EngineFactory engineFactory = new EngineFactory(system);

        BIPGlue glue = new TwoSynchronGlueBuilder()
        // Temporarily commented for parsing purposes
        // {
        //     // @  //Override
        //     public void configure() {
        //         synchron(Measurer.class, SEND_MEASUREMENT).to(Ticker.class, RECEIVE_VALID_MEASUREMENT);
        //         synchron(Measurer.class, SEND_MEASUREMENT).to(Ticker.class, RECEIVE_INVALID_MEASUREMENT);
        //         data(Measurer.class, OUTGOING_MEASUREMENT).to(Ticker.class, INCOMING_MEASUREMENT);
        //     }
        // }
        .build();

        BIPEngine engine = engineFactory.create(ENGINE, glue);
        engine.register(new Measurer(), MEASURER_SPEC, true);
        engine.register(new Ticker(), TICKER_SPEC, true);

        engine.start();
        engine.execute();

        while (true) {
            // Run infinitely
        }
    }
}

/**
 * Measure.java
 */

@ComponentType(initial = NO_MEASURE, name = MEASURER)
@Invariant("v != null ? v >= 0 : true")
@StatePredicate(state = NO_MEASURE, predicate = "v == null")
@StatePredicate(state = HAS_MEASURE, predicate = "v != null")
class Measurer {
    private int v;

    @Transition(name = DO_MEASUREMENT, source = NO_MEASURE, target = HAS_MEASURE,
        pre = "true", post = "true")
    public void doMeasurement() {
        // Bug: v might be negative
        v = ThreadLocalRandom().current().nextInt();
    }

    @Transition(name = SEND_MEASUREMENT, source = HAS_MEASURE, target = NO_MEASURE,
        pre = "v != null", post = "v == null")
    public void sendMeasurement() {
        v = null;
    }

    @Data(name = OUTGOING_MEASUREMENT)
    public Integer getCurrentMeasurement() {
        return v;
    }
}

/**
 * Ticker.java
 */

@ComponentType(initial = ON, name = TICKER)
@StatePredicate(state = ON, predicate = "numMeasurements >= 0 && numHigh >= 0 && numMeasurements >= numHigh")
@StatePredicate(state = CRASHED, predicate = "true")
class Ticker {
    private int numMeasurements;
    private int numHigh;

    @Transition(name = RECEIVE_VALID_MEASUREMENT, source = ON, target = ON,
        guard = IS_VALID_MEASUREMENT, // This is duplicate with the precondition, but I had initially forgotten it, so there might be cases where this is desired. E.g. what if guard implements the wrong check?
        pre = "v >= 0",
        post = "numMeasurements == old(numMeasurements) + 1 && (v > 100 ? high == old(high) + 1 : high == old(high))")
    public void receiveValidMeasurement(@Data(name = INCOMING_MEASUREMENT) int v) {
        System.out.println("Positive measurement received: " + v);
        numMeasurements += 1;
        if (v > 100) {
            System.out.println("- This is a high measurement!");
            numHigh += 1;
        }
    }

    @Transition(name = RECEIVE_INVALID_MEASUREMENT, source = ON, target = CRASHED,
        guard = IS_INVALID_MEASUREMENT,
        pre = "v < 0",
        post = "true")
    public void receiveInvalidMeasurement(@Data(name = INCOMING_MEASUREMENT) int v) {
        System.out.println("Negative measurement received: " + v + ", crashing");
    }

    @Guard(name = IS_INVALID_MEASUREMENT)
    @Pure
    public boolean isInvalidMeasurement(@Data(name = INCOMING_MEASUREMENT) int v) {
        return v < 0;
    }

    @Guard(name = IS_VALID_MEASUREMENT)
    @Pure
    public boolean isValidMeasurement(@Data(name = INCOMING_MEASUREMENT) int v) {
        return v >= 0;
    }
}

