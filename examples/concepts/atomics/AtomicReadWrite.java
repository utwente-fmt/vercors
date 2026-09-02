// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases AtomicReadWriteWitnesses
//:: tools chalice
//:: options --explicit
/**
 The command line to verify with the VerCors Tool is:

 vct --chalice --explicit AtomicReadWrite.java

 The expected result is Pass.
 */
class AtomicReadWrite {

    int x;
    int v;

    //@ resource state()=Perm(x,1) ** x%2 == 0;
    //@ resource lastseen(int id,int i);

    /*@
      given int id;
      given int i;
      requires (lastseen(id,i)) ** state();
      ensures  lastseen(id,v);
    @*/
    public void zet(int v);

    /*@
      given int id;
      given int i;
      requires lastseen(id,i);
      ensures  (lastseen(id,\result)) ** ((\result!=i && \result==id) ==> state());
    @*/
    int get();

    /*@ requires lastseen(1,-1); @*/
    void run1(){
        int v;
        //@ ghost int last=-1;
        //@ loop_invariant lastseen(1,last);
        //@ loop_invariant last!=1;
        while(true) {
            v=this.get() /*@ given { id=1, i=last} @*/;
            //@ ghost last=v;
            if(v==1){
                //@ unfold state();
                x=x+1;
                x=x+1;
                //@ fold state();
                this.zet(2) /*@ given { id=1, i=last } @*/;
                //@ ghost last=2;
            }
        }
    }

    /*@ requires lastseen(2,-1);  @*/
    void run2(){
        boolean b;
        int v;
        //@ ghost int last=-1;
        //@ loop_invariant lastseen(2,last);
        //@ loop_invariant last!=2;
        while(true) {
            v = this.get() /*@ given { id=2, i=last }@*/;
            //@ ghost last=v;
            if(v==2){
                //@ unfold state();
                b = x%2==0;
                //@ fold state();
                //@ assert b;
                this.zet(1) /*@ given { id=2, i=last} @*/;
                //@ ghost last=1;
            }
        }
    }
}

