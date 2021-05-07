//:: cases ControlFlowCatchIf
//:: tools silicon
//:: verdict Pass

class ControlFlowCatchIf {
    boolean randomBoolean();

    void m () {
        int x = 0;
        boolean throwB = randomBoolean();
        boolean ifB = randomBoolean();

        try {
            if (throwB) {
                throw new Exception();
            }
        } catch (Exception e) {
            if (ifB) {
                x = 3;
            } else {
                x = 5;
            }
        }

        if (throwB) {
            if (ifB) {
                assert x == 3;
            } else {
                assert x == 5;
            }
        } else {
            assert x == 0;
        }
    }
}


