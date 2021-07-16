//:: cases ContractSatisfiableIntentionalJava
//:: tools silicon
//:: verdict Pass

class MyClass {
    // User indicates false assumption is required
    //@ requires false;
    void foo() {
        // Should therefore not be triggered.
        //@ assert 5 == 6;
    }
}
