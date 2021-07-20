//:: cases ContinueBreakPass
//:: tools silicon
//:: verdict Pass

class CB {
    boolean p();

    void m1() {
        boolean pp = p();
        while (pp) {
            continue;
            assert false;
        }
    }
}