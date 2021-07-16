// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases JavaOverloading
//:: tools silicon


class Overloading {

    static int main;
    int instance;

    void m(int x) {
        //@ assert 1+1 == 2;
    }

    void m(float x) {
        //@ assert 1+1 == 2;
    }

    void m(boolean... x) {
        //@ assert 1+1 == 2;
    }

    /*@
      context Perm(main,write);
      context Perm(instance,write);
    @*/
    void main(int i) {
        m(1);
        m((float) 1);
        //m(true,false);
        main(42);
        main = 37;
        instance = 42;
    }
}

