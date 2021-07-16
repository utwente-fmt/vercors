// -*- tab-width:4 ; indent-tabs-mode:nil -*-
//:: cases Knuth_Tabulate
//:: tools silicon
//:: verdict Pass

final class MyClass {
    int i;

    //@ context Perm(i, 1);
    //@ ensures i == \old(i) + 1;
    int read_char();

    void write_char(int c);

    void return_the_carriage();

    void tabulate();

    //@ context Perm(i, 1);
    void foo() {
        //@ ghost int old_i = i;
        int SLASH = 900;
        int DOT = 100;
        int x = read_char();
        //@ assert old_i + 1 == i;
        //@ ghost boolean saw_slash = false;
        char_processed:
        {
            if (x == SLASH) {
                //@ ghost saw_slash = true;
                x = read_char();
                if (x == SLASH) {
                    return_the_carriage();
                    break char_processed;
                } else {
                    tabulate();
                }
            }
            write_char(x);
            if (x == DOT) {
                write_char(DOT);
            }
        }
        //@ assert saw_slash ? old_i + 2 == i : old_i + 1 == i;
    }
}
