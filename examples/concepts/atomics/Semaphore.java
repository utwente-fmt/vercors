// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases Semaphore
//:: tools chalice
//:: verdict Pass
//:: options --explicit

/*
 Example: Semaphore
 Description: Semaphore using AtomicInteger as synchronizer.
 Author: Afshin Amighi
 Status: Pass.
 command: vct --chalice --explicit Semaphore.java
 */

public class Semaphore{

    // partd resource
    private int data;

    // constants are not supported
    // ghost final int S=0;

    // maximum number of thread concurrently using the partd resource (data).
    //@ ghost int permits;

    // resource invariant
    //@ resource inv(zfrac p) = Perm(data,p);
    //@ ghost pure zfrac part(int r, int v, int M){ return (r==0 && v>=0 && M>0 && v<=M && M<=100) ? (v\M) : 0; }
    //@ ghost pure zfrac min(zfrac l, zfrac r){ return (l-r<0) ? 0:l-r; }

    /* ------- AtomicInteger ------------*/
    /* only the methods and contracts used in the verification. */

    //@ requires true;
    //@ ensures true;
    public int get();

    //@ given int max;
    //@ requires max >0 && x <=max && n<=max && 100%max==0;
    //@ requires inv(min(part(0,n,max),part(0,x,max)));
    //@ ensures  \result ==> inv(min(part(0,x,max),part(0,n,max)));
    //@ ensures !\result ==> inv(min(part(0,n,max),part(0,x,max)));
    public boolean compareAndSet(int x, int n);

    //@ requires Value(permits) ** permits>0 ** permits <=100 ** 100%permits==0;
    //@ ensures !\result ==> Value(permits) ** permits>0 ** permits <=100 ** 100%permits==0;
    //@ ensures \result ==> Value(permits) ** permits <=100 ** permits>0  **  inv(100\permits\100);
    private boolean tryAcquire(){
        boolean r = false;
        int c = get();
        if( c > 0 ){
            int nextc = c-1;
            //@ assume c <= permits;

            //@    fold  inv(min(part(0,nextc,permits),part(0,c,permits)));
            r = compareAndSet(c,nextc) /*@ given { max=permits } @*/;
            //@    assert c\permits-nextc\permits == 100\permits\100;

            /*@
             ghost if(!r) {
                unfold  inv(min(part(0,nextc,permits),part(0,c,permits)));
             }
             @*/

        }
        return r;
    }




    //@ requires Value(permits)**permits>0** permits<=100 ** 100%permits==0;
    //@ ensures Value(permits)**permits>0** permits<=100 ** 100%permits==0 ** inv(100\permits\100);
    public void doAcquire(){
        boolean stop = tryAcquire();
        /*
         if(stop)
            dae = tri;
         */

        //@ loop_invariant stop ==> Value(permits)** permits>0 ** permits<=100 ** 100%permits==0 ** inv(100\permits\100);
        //@ loop_invariant !stop ==> Value(permits)** permits>0 ** permits<=100 ** 100%permits==0;
        while(!stop) {
            stop = tryAcquire() ;
        }
    }



    //@ requires Value(permits) ** permits>0 ** permits<=100 ** 100%permits==0 ** inv(100\permits\100);
    //@ ensures \result ==> Value(permits) ** permits>0 ** permits<=100;
    //@ ensures !\result ==> Value(permits) ** permits>0 ** permits<=100 ** inv(100\permits\100);
    private boolean tryRelease(){
        boolean r = false;
        int c = get();
        //@ assume c<permits && c>=0;
        int nextc = c+1;
        //@  assert nextc\permits-c\permits == 100\permits\100;
        r = compareAndSet(c,nextc) /*@ given { max = permits } @*/;
        /*@ ghost if(r){
            unfold inv(min(part(0,c,permits),part(0,nextc,permits)));
         }
         @*/
        return r;
    }


    //@ requires Value(permits)**permits>0 ** permits<=100 ** 100%permits==0 ** inv(100\permits\100);
    //@ ensures Value(permits)**permits>0 ** permits<=100 ** 100%permits==0;
    public void doRelease(){
        boolean stop = tryRelease();

        //@ loop_invariant (!stop) ==> Value(permits)**permits>0**permits<=100**100%permits==0**inv(100\permits\100);
        //@ loop_invariant (stop) ==> Value(permits)**permits>0**permits<=100**100%permits==0;
        while(!stop) {
            stop = tryRelease();
        }
    }

}
