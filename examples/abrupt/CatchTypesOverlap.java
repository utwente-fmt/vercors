// -*- tab-width:4 ; indent-tabs-mode:nil -*-
//:: cases CatchTypesOverlap
//:: tools silicon
//:: verdict Error

class MyClass {
    void foo() {
        try {
            throw new Exception();
        } catch (Exception e) {

        } catch (Exception f) {

        }
    }
}