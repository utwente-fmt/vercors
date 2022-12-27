package casino;

public class Constants {
    public static final String ACTOR_SYSTEM = "ACTOR_SYSTEM";
    public static final String ENGINE = "ENGINE";
    public static final String CASINO_SPEC = "CASINO_SPEC";
    public static final String CASINO = "CASINO";
    public static final String OPERATOR_SPEC = "OPERATOR_SPEC";
    public static final String PLAYER_SPEC = "PLAYER_SPEC";

    //states
    public static final String OFF = "OFF";
    public static final String IDLE = "IDLE";
    public static final String GAME_AVAILABLE = "GAME_AVAILABLE";
    public static final String BET_PLACED  = "BET_PLACED";
    public static final String BET_PREPARED  = "BET_PREPARED";
    public static final String PUT_FUNDS = "PUT_FUNDS";
    public static final String WITHDRAW_FUNDS = "WITHDRAW_FUNDS";
    public static final String WORKING = "WORKING";

    //transitions
    public static final String REMOVE_FROM_POT = "REMOVE_FROM_POT";
    public static final String ADD_TO_POT = "ADD_TO_POT";
    public static final String CREATE_GAME = "CREATE_GAME";
    public static final String PLACE_BET = "PLACE_BET";
    public static final String PREPARE_BET = "PREPARE_BET";
    public static final String RECEIVE_BET = "RECEIVE_BET";
    public static final String DECIDE_BET = "DECIDE_BET";
    public static final String RECEIVE_MONEY = "RECEIVE_MONEY";
    public static final String CASINO_WIN  = "CASINO_WIN";
    public static final String PLAYER_WIN  = "PLAYER_WIN";
    public static final String PREPARE_TO_ADD = "PREPARE_TO_ADD";
    public static final String PREPARE_TO_REMOVE = "PREPARE_TO_REMOVE";
    public static final String OPEN_CASINO = "OPEN_CASINO";
    public static final String CLOSE_CASINO = "CLOSE_CASINO";

    //data
    public static final String OPERATOR = "OPERATOR";
    public static final String PLAYER = "PLAYER";
    public static final String ID = "ID";
    public static final String OUTGOING_GUESS = "OUTGOING_GUESS";
    public static final String OUTGOING_BET = "OUTGOING_BET";
    public static final String INCOMING_GUESS = "INCOMING_GUESS";
    public static final String INCOMING_BET = "INCOMING_BET";
    public static final String OUTGOING_MONEY = "OUTGOING_MONEY";
    public static final String INCOMING_MONEY = "INCOMING_MONEY";
    public static final String OUTGOING_FUNDS = "OUTGOING_FUNDS";
    public static final String INCOMING_FUNDS = "INCOMING_FUNDS";
    public static final String AVAILABLE_FUNDS = "AVAILABLE_FUNDS";
    public static final String CASINO = "CASINO";

    //guards
    public static final String IS_PLAYER = "IS_PLAYER";
    public static final String IS_OPERATOR = "IS_OPERATOR";
    public static final String IS_NOT_OPERATOR = "IS_NOT_OPERATOR";
    public static final String IS_CASINO = "IS_CASINO";
    public static final String GUESSED = "GUESSED";
    public static final String ENOUGH_FUNDS = "ENOUGH_FUNDS";
    public static final String ALLOWABLE_BET = "ALLOWABLE_BET";
    public static final String SAFE_GAME_STEP = "SAFE_GAME_STEP";
}

enum Coin { HEADS, TAILS }
