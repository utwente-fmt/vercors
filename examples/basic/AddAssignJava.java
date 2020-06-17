//:: cases AddAssignJava
//:: tools silicon
//:: verdict Pass

class AddAssignJava {
    void test() {
        int x = 5;
        //@ assert x == 5;
        x += 2;
        //@ assert x == 7;
    }
}