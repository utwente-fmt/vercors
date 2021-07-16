// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases AtomicReadWriteWitnesses
//:: tools chalice
//:: options --explicit

/**
 * The command line to verify with the VerCors Tool is:
 * <p>
 * vct --chalice --explicit AtomicReadWrite.java
 * <p>
 * The expected result is Pass.
 */
class AtomicReadWrite {

    int x;
    int v;

    //@ resource state()=Perm(x,100) ** x%2 == 0;
    //@ resource lastseen(int id,int i);

    /*@
      given int id,i;
      requires (req_ls:lastseen(id,i)) ** req_st:state();
      ensures  ens_ls:lastseen(id,v);
    @*/
    public void set(int v);

    /*@
      given int id,i;
      requires req_ls:lastseen(id,i);
      ensures  (ens_ls:lastseen(id,\result)) ** ((\result!=i && \result==id) ==> ens_st:state());
    @*/
    int get();

    /*@
      requires req_ls:lastseen(1,-1);
    @*/
    void run1() {
        int v;
        //@ witness tmp:state();
        //@ int last=-1;
        //@ loop_invariant inv:lastseen(1,last);
        //@ loop_invariant last!=1;
        while (true)/*@ with { inv = req_ls ; } */ {
            v = this.get() /*@ with { id=1; i=last; req_ls=inv; }  then { inv=ens_ls; tmp=ens_st; } @*/;
            //@ last=v;
            if (v == 1) {
                //@ unfold tmp:state();
                x = x + 1;
                x = x + 1;
                //@ fold tmp:state();
                this.set(2) /*@ with { id=1; i=last; req_ls=inv; req_st=tmp; }  then { inv=ens_ls; } @*/;
                //@ last=2;
            }
        }
    }

    /*@
      requires req_ls:lastseen(2,-1);
    @*/
    void run2() {
        boolean b;
        int v;
        //@ witness tmp:state();
        //@ int last=-1;
        //@ loop_invariant inv:lastseen(2,last);
        //@ loop_invariant last!=2;
        while (true)/*@ with { inv = req_ls ; } */ {
            v = this.get() /*@ with { id=2; i=last; req_ls=inv; } then { inv=ens_ls; tmp=ens_st; } @*/;
            //@ last=v;
            if (v == 2) {
                //@ unfold tmp:state();
                b = x % 2 == 0;
                //@ fold tmp:state();
                //@ assert b;
                this.set(1) /*@ with { id=2; i=last; req_ls=inv; req_st=tmp; }  then { inv=ens_ls; } @*/;
                //@ last=1;
            }
        }
    }

}

