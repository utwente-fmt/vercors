// -*- tab-width:4 ; indent-tabs-mode:nil -*-
//:: cases LabeledStatements
//:: tools silicon
//:: verdict Pass

class MyClass {
    void ex1() {
        LABEL: try {
           // Some code.
        } catch(Exception e) {
           if(condition) {
              break LABEL;
           }
           // Remaining code.
        }
    }

    void ex2() {
        LABEL: if (condition()) {
            if (condition2()) {
                break LABEL;
            }
        }
    }
}
