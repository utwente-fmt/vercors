// -*- tab-width:4 ; indent-tabs-mode:nil -*-
//:: cases For
//:: tools silicon
//:: verdict Pass

public class For {
    /*@ requires n >= 0;
        ensures \result == n * (n+1) / 2;
      @*/
    public int test(int n) {
        int total = 0;

        /*@ loop_invariant total == i * (i-1) / 2;
          @*/
        for(int i = 1; i != n+1; i++)
        {
            total = total + i;
        }

        return total;
    }
}