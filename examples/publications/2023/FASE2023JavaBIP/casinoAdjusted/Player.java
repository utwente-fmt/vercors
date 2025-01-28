package casino;

import org.javabip.annotations.*;
import org.javabip.api.PortType;
import static casino.Coin.HEADS;
import static casino.Coin.TAILS;
import static casino.Constants.*;

@Port(name = PREPARE_BET, type = PortType.enforceable)
@Port(name = PLACE_BET, type = PortType.enforceable)
@Port(name = RECEIVE_MONEY, type = PortType.enforceable)
@ComponentType(initial = GAME_AVAILABLE, name = PLAYER_SPEC)
@Invariant("purse >= 0 && id != null")
@StatePredicate(state = BET_PREPARED, expr = "guess != null && bet >= 0")
public class Player {
    final Integer id;
    int bet;
    Coin guess;
    int purse;

    //@ requires purse >= 0;
    Player(int id, int purse) {
        this.id = new Integer(id);
        this.purse = purse;
        //@ ghost System.staticInvariant();
        System.out.println("PLAYER" + this.id.toString() + ": INITIALIZED");
    }
    
    // Player prepares a bet
    @Transition(name = PREPARE_BET, source = GAME_AVAILABLE, target = BET_PREPARED)
    public void prepareBet() {
        bet = (int) (Math.random() * purse);
        guess = Math.random() < 0.5 ? HEADS : TAILS;
        purse = purse - bet;
        //@ ghost System.staticInvariant();
        System.out.println("PLAYER" + id.toString()
                + ": bet " + new Integer(bet).toString()
                + " prepared, purse: " + new Integer(purse).toString());
    }

    // Player places a bet
    @Transition(name = PLACE_BET, source = BET_PREPARED, target = GAME_AVAILABLE)
    public void placeBet() {
        //@ ghost System.staticInvariant();
        System.out.println("PLAYER" + id.toString()
                + ": bet " + new Integer(bet).toString()
                + " placed, purse: " + new Integer(purse).toString());
        bet = 0;
        guess = null;
    }

    // Player receives a contribution
    @Transition(name = RECEIVE_MONEY, source = GAME_AVAILABLE, target = GAME_AVAILABLE,
        requires = "win >= 0" // Needed, otherwise VerCors cannot prove invariant again. E.g. what if win is negative? This is excluded by the casino invariant "bet >= 0"
    )
    public void receiveContribution(@Data(name = INCOMING_MONEY) int win) {
        purse += win;
        //@ ghost System.staticInvariant();
        System.out.println("PLAYER" + id.toString()
                + ": won " + new Integer(win).toString()
                + " purse: " + new Integer(purse).toString());
    }

    @Data(name = OUTGOING_BET)
    @Pure
    public int getBet() {
        return bet;
    }

    @Data(name = OUTGOING_GUESS)
    @Pure
    public Coin getGuess() {
        return guess;
    }

    @Data(name = ID)
    @Pure
    public Integer id() {
        return id;
    }
}
