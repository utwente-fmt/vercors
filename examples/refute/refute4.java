// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases Refute4
//:: tools silicon
//:: verdict Pass

/*
 The refute statement is reachable, therefore it is valid.
*/
public class Refute {

    int x;

    /*@
      requires Perm(x,write) ** x==2;
      ensures  Perm(x,write) ** x==3;
    @*/
    public void good2() {
        //@ refute x==3;
        x = 3;
    }

}

