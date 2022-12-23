//:: cases ContinueBreakFail
//:: tools silicon
//:: verdict Fail

class CB {
    boolean p();

    void m2() {
        boolean pp = p();
        while (pp) {
            break;
        }
        /*[/expect assertFailed:false]*/
        assert false; // Should be triggered
        /*[/end]*/
    }
}