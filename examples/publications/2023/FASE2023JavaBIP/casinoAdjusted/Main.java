/*
This version of the casino case study adjusts the contracts, by including wherever needed that the pot needs to be big
enough. This fixes verification problems, at the cost of introducing a deadlock.
*/

package casino;

import akka.actor.ActorSystem;
import org.javabip.api.BIPEngine;
import org.javabip.api.BIPGlue;
import org.javabip.engine.factory.EngineFactory;
import org.javabip.glue.TwoSynchronGlueBuilder;
import static casino.Constants.*;

public class Main {
    public static void main(String[] args) throws Exception {
        ActorSystem system = ActorSystem.create(ACTOR_SYSTEM);
        EngineFactory engineFactory = new EngineFactory(system);

        BIPGlue glue = new /*@ vercorsBipJob @*/ TwoSynchronGlueBuilder() {
            @Override
            public void configure() {
                port(Operator.class, PREPARE_TO_ADD).requiresNothing();
                port(Operator.class, PREPARE_TO_ADD).acceptsNothing();
                port(Operator.class, PREPARE_TO_REMOVE).requiresNothing();
                port(Operator.class, PREPARE_TO_REMOVE).acceptsNothing();

                port(Player.class, PREPARE_BET).requiresNothing();
                port(Player.class, PREPARE_BET).acceptsNothing();

                synchron(Operator.class, CREATE_GAME).to(Casino.class, CREATE_GAME);
                synchron(Operator.class, ADD_TO_POT).to(Casino.class, ADD_TO_POT);
                synchron(Operator.class, REMOVE_FROM_POT).to(Casino.class, REMOVE_FROM_POT);

                synchron(Casino.class, RECEIVE_BET).to(Player.class, PLACE_BET);

                port(Operator.class, DECIDE_BET).requires(Casino.class, CASINO_WIN); 
                port(Casino.class, CASINO_WIN).requires(Operator.class, DECIDE_BET);

                port(Operator.class, DECIDE_BET).requires(Casino.class, PLAYER_WIN);
                port(Casino.class, PLAYER_WIN).requires(Operator.class, DECIDE_BET);
                port(Player.class, RECEIVE_MONEY).requires(Casino.class, PLAYER_WIN);

                port(Casino.class, CASINO_WIN).accepts(Operator.class, DECIDE_BET);
                port(Casino.class, PLAYER_WIN).accepts(Operator.class, DECIDE_BET, Player.class, RECEIVE_MONEY);
                port(Operator.class, DECIDE_BET).accepts(Casino.class, CASINO_WIN, Casino.class, PLAYER_WIN, Player.class, RECEIVE_MONEY);
                port(Player.class, RECEIVE_MONEY).accepts(Casino.class, PLAYER_WIN, Operator.class, DECIDE_BET);

                data(Operator.class, OUTGOING_FUNDS).to(Casino.class, INCOMING_FUNDS);
                data(Operator.class, ID).to(Casino.class, OPERATOR);

                data(Player.class, OUTGOING_BET).to(Casino.class, INCOMING_BET);
                data(Player.class, OUTGOING_GUESS).to(Casino.class, INCOMING_GUESS);
                data(Player.class, ID).to(Casino.class, PLAYER);

                data(Casino.class, OUTGOING_MONEY).to(Player.class, INCOMING_MONEY);
                data(Casino.class, AVAILABLE_FUNDS).to(Operator.class, AVAILABLE_FUNDS);
            }
        }.build();

        BIPEngine engine = engineFactory.create(ENGINE, glue);

        for (int id = 1; id <= 1; id++) {
            int oid = 100 + id;
            engine.register(new Operator(oid, 500), OPERATOR_SPEC + oid, true);
            int cid = 200 + id;
            engine.register(new Casino(cid,oid), CASINO_SPEC + cid, true);
        }

        for (int id = 1; id <= 3; id++) {
            int pid = 300 + id;
            engine.register(new Player(pid, 100), PLAYER_SPEC + pid, true);
        }

        engine.start();
        engine.execute();

        while (true) {
            // Run infinitely
        }
    }
}
