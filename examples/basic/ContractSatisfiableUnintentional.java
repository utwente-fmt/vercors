//:: cases ContractSatisfiableUnintentionalJava
//:: tools silicon
//:: verdict Fail

class MyClass {
    // User makes a mistake here, this should be detected.
    //@ requires 3 == 4;
    void bar() {
        //@ assert 5 == 6;
    }
}
