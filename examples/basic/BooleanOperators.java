//:: cases AmbiguousBooleanOperators
//:: tools silicon
//:: verdict Pass
class BooleanOperators {
    boolean xor() {
        return true ^ false;
    }

    boolean and() {
        return true & false;
    }

    boolean or() {
        return true | false;
    }
}