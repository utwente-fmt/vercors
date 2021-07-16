// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases HistoryLoop
//:: tools silicon
//:: verdict Pass
//:: options --check-history

class Application {

    /*@
      requires n>=0;
      given process p;
      given frac q;
      requires q != none;
      requires Hist(h,q,p);
      requires HPerm(h.x,1);
      ensures  HPerm(h.x,1);
      ensures  Hist(h,q,p*h.single(n));
    @*/
    void do_incr_loop(History h, int n) {
        int i;
        i = 0;
        //@ loop_invariant 0 <= i ** i <= n ** q != none ** HPerm(h.x,1) ** Hist(h,q,p * h.single(i));
        while (i < n) {
            {
                //@ action h, q , p*h.single(i) , h.incr();
                h.x = h.x + 1;
            }
            i = i + 1;
        }
    }

    /*@
      requires m>=0;
      requires n>=0;
      given frac q;
      requires q != none;
      requires Hist(h,q,empty);
      requires HPerm(h.x,1);
      ensures  HPerm(h.x,1);
      ensures  Hist(h,q,h.single(m+n));
    @*/
    void do_loop_twice(History h, int m, int n) {
        do_incr_loop(h, m) /*@ with { q = q ; p = empty ; } @*/;
        do_incr_loop(h, n) /*@ with { q = q ; p = h.single(m) ; } @*/;
    }

}

