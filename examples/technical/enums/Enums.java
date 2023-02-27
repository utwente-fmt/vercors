enum AB { A, B }

class C {
    void foo() {
        AB ab = AB.A;
        //@ assert AB.A != null;
        //@ assert ab != AB.B;
    }

    //@ requires ab != null;
    void bar(AB ab) {
        //@ assert ab == AB.A || ab == AB.B;
        AB[] abs = new AB[2];
        abs[0] = ab;
        abs[1] = AB.A;
    }
}
