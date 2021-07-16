// -*- tab-width:4 ; indent-tabs-mode:nil -*-
//:: cases LabeledStatements
//:: tools silicon
//:: verdict Pass

class MyClass {
    boolean p();

    void ex1() {
        LABEL:
        try {
            // Some code.
        } catch (Exception e) {
            boolean pp = p();
            if (pp) {
                break LABEL;
            }
            // Remaining code.
        }
    }

    void ex2() {
        boolean pp = p();
        LABEL:
        if (pp) {
            pp = p();
            if (pp) {
                break LABEL;
            }
        }
    }
}
