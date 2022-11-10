package casino;

import org.javabip.annotations.*;
import org.javabip.api.DataOut;
import org.javabip.api.PortType;

import static casino.Constants.*;

@Port(name = CREATE_GAME, type = PortType.enforceable)
@Port(name = ADD_TO_POT, type = PortType.enforceable)
@Port(name = REMOVE_FROM_POT, type = PortType.enforceable)
@Port(name = DECIDE_BET, type = PortType.enforceable)
@Port(name = PREPARE_TO_ADD, type = PortType.enforceable)
@Port(name = PREPARE_TO_REMOVE, type = PortType.enforceable)
@ComponentType(initial = WORKING, name = OPERATOR_SPEC)
@Invariant("pot >= 0 && wallet >= 0")
@StatePredicate(state = PUT_FUNDS, expr = "amountToMove >= 0")
@StatePredicate(state = WITHDRAW_FUNDS, expr = "0 <= amountToMove && amountToMove <= pot")
public class Operator {
    final int id;
    int wallet;
    int pot;
    int amountToMove;

    Operator (int id, int funds) {
        this.id = id;
        wallet = funds;
        amountToMove = 0;
        System.out.println("OPERATOR" + id + " created with wallet: " + wallet);
    }

    @Transition(name = CREATE_GAME, source = WORKING, target = WORKING)
    @Transition(name = CREATE_GAME, source = PUT_FUNDS, target = PUT_FUNDS)
    @Transition(name = CREATE_GAME, source = WITHDRAW_FUNDS, target = WITHDRAW_FUNDS)
    @Transition(name = DECIDE_BET, source = WORKING, target = WORKING) 
    @Transition(name = DECIDE_BET, source = PUT_FUNDS, target = PUT_FUNDS)
    @Transition(name = DECIDE_BET, source = WITHDRAW_FUNDS, target = WITHDRAW_FUNDS) // TODO: Problem here. Say: Operator.amountToMove == 100, Casino.pot = 10. Then DECIDE_BET executes, sets Operator.pot to 10. Now if REMOVE_FROM_POT is executed, we get negative funds. I think VerCors will give an error here, since it won't be able to prove the state predicate of WITHDRAW_FUNDS. But: this means operator can deadlock?
    public void gameStep(@Data(name = AVAILABLE_FUNDS) int pot) {
        this.pot = pot;
        System.out.println("OPERATOR" + id + ": making one step in the game");
    }

    @Transition(name = PREPARE_TO_ADD, source = WORKING, target = PUT_FUNDS, guard = ENOUGH_FUNDS)
    public void prepareAmountToPut() {
        amountToMove = (int) (Math.random() * wallet) + 1;
        wallet -= amountToMove;
        System.out.println("OPERATOR" + id + ": decided to put " + amountToMove + ", wallet: " + wallet);
    }

    @Transition(name = PREPARE_TO_REMOVE, source = WORKING, target = WITHDRAW_FUNDS)
    public void prepareAmountToWithdraw() {
        amountToMove = (int) (Math.random() * pot) + 1; // TODO: this is wrong? Pot might become -1
        System.out.println("OPERATOR" + id + ": decided to withdraw " + amountToMove + ", wallet: " + wallet);
    }

    @Transition(name = ADD_TO_POT, source = PUT_FUNDS, target = WORKING)
    public void addToPot (@Data(name = AVAILABLE_FUNDS) int pot) {
        this.pot = pot + amountToMove;
        System.out.println("OPERATOR" + id + ": added " + amountToMove + " to pot, wallet: " + wallet);
    }

    @Transition(name = REMOVE_FROM_POT, source = WITHDRAW_FUNDS, target = WORKING)
    public void removeFromPot (@Data(name = AVAILABLE_FUNDS) int pot) {
        wallet += amountToMove;
        this.pot = pot - amountToMove;
        System.out.println("OPERATOR" + id + ": removed " + amountToMove + " from pot, wallet: " + wallet);
    }

    @Pure
    @Guard(name = ENOUGH_FUNDS)
    public boolean haveMoney() {
        return wallet > 0;
    }

    @Pure
    @Data(name = OUTGOING_FUNDS, accessTypePort = /*@ \replacing(0) */ DataOut.AccessType.allowed /*@ \replacing_done */, ports = {ADD_TO_POT, REMOVE_FROM_POT})
    public int funds() {
        return amountToMove;
    }

    @Pure
    @Data(name = ID)
    public int id() {
        return id;
    }
}
