// -*- tab-width:4 ; indent-tabs-mode:nil -*-
//:: cases CatchTypesOverlap
//:: tools silicon
//:: verdict Fail

class MyClass {
    void foo() {
        try {
            throw new Exception();
        } catch (Exception e) {

        } catch (Exception f) {

        }

        //@ assert false;
    }
}