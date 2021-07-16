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

public class Semaphore {

    // partd resource
    private int data;

    // constants are not supported
    // ghost final int S=0;

    // maximum number of thread concurrently using the partd resource (data).
    //@ int permits;

    // resource invariant
    //@ resource inv(zfrac p) = Perm(data,p);
    //@ pure zfrac part(int r, int v, int M){ return (r==0 && v>=0 && M>0 && v<=M && M<=100) ? ((v*100)/M) : (0); }

    /* ------- AtomicInteger ------------*/
    /* only the methods and contracts used in the verification. */

    //@ requires true;
    //@ ensures true;
    public int get();

    //@ given int max;
    //@ requires max >0 && x <=max && n<=max && 100%max==0;
    //@ requires cri:inv(part(0,n,max)-part(0,x,max));
    //@ ensures  \result ==> cepi:inv(part(0,x,max)-part(0,n,max));
    //@ ensures !\result ==> ceni:inv(part(0,n,max)-part(0,x,max));
    public boolean compareAndSet(int x, int n);

    /*---------- Semaphore --------------*/

    //@ requires Value(permits) ** permits>0 ** permits <=100 ** 100%permits==0;
    //@ ensures !\result ==> Value(permits) ** permits>0 ** permits <=100 ** 100%permits==0;
    //@ ensures \result ==> Value(permits) ** permits <=100 ** permits>0  ** tae: inv(100/permits);
    private boolean tryAcquire() {
        boolean r = false;
        //@ witness tcri:inv(*);
        //@ witness tcepi:inv(*);
        //@ witness tceni:inv(*);
        int c = get();
        if (c > 0) {
            int nextc = c - 1;
            //@ assume c <= permits;

            //@    fold tcri: inv(part(0,nextc,permits)-part(0,c,permits));
            r = compareAndSet(c, nextc) /*@ with { max=permits; cri = tcri; } then { tcepi = cepi; tceni = ceni; } @*/;
            //@    assert (c*100)/permits-(nextc*100)/permits == 100/permits;

            /*@
             if(r){
                tae = tcepi;
             }else{
                unfold tceni: inv(part(0,nextc,permits)-part(0,c,permits));
             }
             @*/

        }
        return r;
    }

    //@ requires Value(permits)**permits>0** permits<=100 ** 100%permits==0;
    //@ ensures Value(permits)**permits>0** permits<=100 ** 100%permits==0 ** dae:inv(100/permits);
    public void doAcquire() {
        //@ witness tri:inv(*);
        boolean stop = tryAcquire() /*@ with{} then{ tri = tae; } @*/;
        /*
         if(stop)
            dae = tri;
         */

        //@ loop_invariant stop ==> Value(permits)** permits>0 ** permits<=100 ** 100%permits==0 ** lri:inv(100/permits);
        //@ loop_invariant !stop ==> Value(permits)** permits>0 ** permits<=100 ** 100%permits==0;
        while (!stop) /*@ with { lri=tri; } then { dae =lri ;} @*/ {
            stop = tryAcquire() /*@ with {} then { lri=tae; } @*/;
        }
    }

    //@ requires Value(permits) ** permits>0 ** permits<=100 ** 100%permits==0 ** trr:inv(100/permits);
    //@ ensures \result ==> Value(permits) ** permits>0 ** permits<=100;
    //@ ensures !\result ==> Value(permits) ** permits>0 ** permits<=100 ** tre:inv(100/permits);
    private boolean tryRelease() {
        boolean r = false;
        //@ witness tcri:inv(*);
        //@ witness tcepi:inv(*);
        //@ witness tceni:inv(*);
        int c = get();
        //@ assume c<permits && c>=0;
        int nextc = c + 1;
        //@    assert (nextc*100)/permits-(c*100)/permits == 100/permits;
        // fold tcri: inv(part(0,nextc,permits)-part(0,c,permits));
        r = compareAndSet(c, nextc) /*@ with { max = permits; cri = trr; } then { tcepi = cepi; tceni = ceni; } @*/;
        /*@ if(r){
            unfold tcepi:inv(part(0,c,permits)-part(0,nextc,permits));
         }else{
             tre=tceni;
         }@*/
        return r;
    }

    //@ requires Value(permits)**permits>0 ** permits<=100 ** 100%permits==0 ** dar:inv(100/permits);
    //@ ensures Value(permits)**permits>0 ** permits<=100 ** 100%permits==0;
    public void doRelease() {
        //@ witness tri:inv(*);
        boolean stop = tryRelease() /*@ with { trr = dar; } then { tri=tre; } @*/;
        /*
         if(!stop)
            dae = tri;
         */

        //@ loop_invariant (!stop) ==> Value(permits)**permits>0**permits<=100**100%permits==0**lri:inv(100/permits);
        //@ loop_invariant (stop) ==> Value(permits)**permits>0**permits<=100**100%permits==0;
        while (!stop) /*@ with { lri = tri; } then {} @*/ {
            stop = tryRelease() /*@ with { trr=lri; } then { lri=tre; } @*/;
        }
    }
}
