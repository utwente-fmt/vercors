package casino;

import org.javabip.annotations.*;
import org.javabip.api.DataOut;
import org.javabip.api.PortType;
import static casino.Coin.*;
import static casino.Constants.*;

@Port(name = ADD_TO_POT, type = PortType.enforceable)
@Port(name = REMOVE_FROM_POT, type = PortType.enforceable)
@Port(name = CREATE_GAME, type = PortType.enforceable)
@Port(name = RECEIVE_BET, type = PortType.enforceable)
@Port(name = CASINO_WIN, type = PortType.enforceable)
@Port(name = PLAYER_WIN, type = PortType.enforceable)
@ComponentType(initial = IDLE, name = CASINO_SPEC)
@Invariant("secretNumber != null && bet >= 0 && pot >= bet") // Moved secretNumber to global invariant, different from original
@StatePredicate(state = IDLE, expr = "bet == 0")
@StatePredicate(state = GAME_AVAILABLE, expr = "bet == 0")
@StatePredicate(state = BET_PLACED, expr = "guess != null")
public class Casino {
    final int id;
    int operator;
    int pot;
    // TODO: HashedNumber is missing?
    Integer secretNumber;
    Integer player;

    Coin guess;
    int bet;

    public Casino(int id, int operator) {
        this.id = id;
        this.operator = operator;
        pot = 0;
        bet = 0;
        System.out.println("CASINO" + id + ": INITIALIZED");
        secretNumber = new Integer(-1); // TODO: Different from original
    }

    // Add money to pot
    @Transition(name = ADD_TO_POT, source = IDLE, target = IDLE, guard = IS_OPERATOR, requires = "funds >= 0" /* different from original */)
    @Transition(name = ADD_TO_POT, source = GAME_AVAILABLE, target = GAME_AVAILABLE, guard = IS_OPERATOR, requires = "funds >= 0")
    @Transition(name = ADD_TO_POT, source = BET_PLACED, target = BET_PLACED, guard = IS_OPERATOR, requires = "funds >= 0")
    public void addToPot(@Data(name = OPERATOR) Integer sender, @Data(name = INCOMING_FUNDS) int funds) {
        pot = pot + funds;
        System.out.println("CASINO" + id + ": " + funds +
                " received from operator " + sender +
                ", pot: " + pot);
    }

    // Remove money from pot
    @Transition(name = REMOVE_FROM_POT, source = IDLE, target = IDLE,
        /* With this guard, cannot establish component invariant pot >= 0. It is not provable that funds <= pot
          (even though we know that the operator ensures this is the case)
         */
        guard = IS_OPERATOR)
    @Transition(name = REMOVE_FROM_POT, source = GAME_AVAILABLE, target = GAME_AVAILABLE, guard = IS_OPERATOR)
    public void removeFromPot(@Data(name = OPERATOR) Integer sender, @Data(name = INCOMING_FUNDS) int funds) {
        pot = pot - funds;
        System.out.println("CASINO" + id + ": " + funds +
                " removed by operator " + sender +
                ", pot: " + pot);
    }

    // Operator opens the game
    @Transition(name = CREATE_GAME, source = IDLE, target = GAME_AVAILABLE, guard = IS_OPERATOR)
    public void createGame(@Data(name = OPERATOR) Integer sender) {
        secretNumber = new Integer((int) (Math.random() * 100));
        System.out.println("CASINO" + id + ": GAME CREATED");
    }

    // Operator receives a bet
    @Transition(name = RECEIVE_BET, source = GAME_AVAILABLE, target = BET_PLACED,
            guard = "IS_NOT_OPERATOR && ALLOWABLE_BET",
            requires = "0 <= bet && guess != null")
    public void receiveBet(@Data(name = PLAYER) Integer sender,
                           @Data(name = INCOMING_GUESS) Coin guess, @Data(name = INCOMING_BET) int bet) {
        player = sender;
        this.guess = guess;
        this.bet = bet;
        System.out.println("CASINO" + id + ": received bet: " + bet +
                ", guess: " + guess +
                " from player " + player);
    }

    @Transition(name = CASINO_WIN, source = BET_PLACED, target = IDLE, guard = "IS_OPERATOR && !GUESSED")
    public void casinoWin(@Data(name = OPERATOR) Integer sender) {
        int won = bet;
        pot = pot + bet;
        bet = 0;
        guess = null;
        player = null;
        System.out.println("CASINO" + id + ": " + won + " won" +
                ", pot: " + pot);
    }

    @Transition(name = PLAYER_WIN, source = BET_PLACED, target = IDLE, guard = "IS_OPERATOR && GUESSED && IS_PLAYER")
    public void playerWin(@Data(name = PLAYER) Integer player, @Data(name = OPERATOR) Integer operator) {
        int lost = bet;
        pot = pot - bet;
        bet = 0;
        guess = null;
        player = null;
        System.out.println("CASINO" + id + ": " + lost + " lost" +
                ", pot: " + pot);
    }

    @Pure
    @Guard(name = GUESSED)
    public boolean isGuessed() {
        Coin secret = (secretNumber.intValue() % 2 == 0) ? HEADS : TAILS;
        return secret == guess;
    }

    @Pure
    @Guard(name = IS_OPERATOR)
    public boolean isOperator(@Data(name = OPERATOR) Integer sender) {
        return sender == operator;
    }

    @Pure
    @Guard(name = IS_NOT_OPERATOR)
    public boolean isNotOperator(@Data(name = PLAYER) Integer sender) {
        return sender != operator;
    }

    @Pure
    @Guard(name = IS_PLAYER)
    public boolean isPlayer(@Data(name = PLAYER) Integer sender) {
        return sender == player;
    }

    @Pure
    @Data(name = OUTGOING_MONEY, accessTypePort = /*@ \replacing(0) */ DataOut.AccessType.allowed /*@ \replacing_done */, ports = {PLAYER_WIN})
    public int getWin() {
        return 2*bet;
    }

    @Pure
    @Data(name = AVAILABLE_FUNDS)
    public int getPot() {
        return pot;
    }
    
    @Pure
    @Guard(name = ENOUGH_FUNDS)
    public boolean enoughFunds(@Data(name = INCOMING_FUNDS) int funds) {
        return funds <= pot; 
    }

    @Pure
    @Guard(name = ALLOWABLE_BET)
    public boolean allowableBet(@Data(name = INCOMING_BET) int bet) {
        return bet <= pot;
    }
}
