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
    final Integer id;
    int wallet;
    int pot;
    int amountToMove;

    Operator (Integer id, int funds) throws Exception {
        this.id = id;
        if (funds < 0) throw new Exception("Cannot have negative funds");
        wallet = funds;
        amountToMove = 0;
        System.out.println("OPERATOR" + id + " created with wallet: " + wallet);
    }

    @Transition(name = CREATE_GAME, source = WORKING, target = WORKING, pre = "newPot >= 0")
    @Transition(name = CREATE_GAME, source = PUT_FUNDS, target = PUT_FUNDS, pre = "newPot >= 0")
    @Transition(name = CREATE_GAME, source = WITHDRAW_FUNDS, target = WITHDRAW_FUNDS, pre = "newPot >= 0", guard = SAFE_GAME_STEP)
    @Transition(name = DECIDE_BET, source = WORKING, target = WORKING, pre = "newPot >= 0")
    @Transition(name = DECIDE_BET, source = PUT_FUNDS, target = PUT_FUNDS, pre = "newPot >= 0")
    @Transition(name = DECIDE_BET, source = WITHDRAW_FUNDS, target = WITHDRAW_FUNDS, pre = "newPot >= 0", guard = SAFE_GAME_STEP)
    public void gameStep(@Data(name = AVAILABLE_FUNDS) int newPot) {
        this.pot = newPot;
        System.out.println("OPERATOR" + id + ": making one step in the game");
    }

    @Transition(name = PREPARE_TO_ADD, source = WORKING, target = PUT_FUNDS, guard = ENOUGH_FUNDS)
    public void prepareAmountToPut() {
        amountToMove = (int) (Math.random() * wallet); // Note: Math.random is replaced with 0 here (temporary workaround for static method access)
        wallet -= amountToMove;
        System.out.println("OPERATOR" + id + ": decided to put " + amountToMove + ", wallet: " + wallet);
    }

    @Transition(name = PREPARE_TO_REMOVE, source = WORKING, target = WITHDRAW_FUNDS)
    public void prepareAmountToWithdraw() {
        amountToMove = (int) (Math.random() * pot); // Note: Math.random is replaced with 0 here (temporary workaround for static method access)
        System.out.println("OPERATOR" + id + ": decided to withdraw " + amountToMove + ", wallet: " + wallet);
    }

    @Transition(name = ADD_TO_POT, source = PUT_FUNDS, target = WORKING, pre = "newPot >= 0")
    public void addToPot (@Data(name = AVAILABLE_FUNDS) int newPot) {
        this.pot = newPot + amountToMove;
        System.out.println("OPERATOR" + id + ": added " + amountToMove + " to pot, wallet: " + wallet);
    }

    @Transition(name = REMOVE_FROM_POT, source = WITHDRAW_FUNDS, target = WORKING, pre = "amountToMove <= newPot")
    public void removeFromPot (@Data(name = AVAILABLE_FUNDS) int newPot) {
        wallet += amountToMove;
        this.pot = newPot - amountToMove;
        System.out.println("OPERATOR" + id + ": removed " + amountToMove + " from pot, wallet: " + wallet);
    }

    @Pure
    @Guard(name = SAFE_GAME_STEP)
    public boolean safeGameStep(@Data(name = AVAILABLE_FUNDS) int newPot) {
        return amountToMove <= newPot;
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
    public Integer id() {
        return id;
    }
}
