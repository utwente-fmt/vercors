// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases CountDownLatch
//:: tools chalice
//:: verdict Pass
//:: options --explicit

/*
 Example: CountDownLatch 
 Description: CountDownLatch using AtomicInteger as synchronizer. 
  The specification is a special case of the contracts provided in the FMCAD'13 paper: p==1/count.
 Author: Afshin Amighi
 Status: Pass.
 command: vct --chalice --explicit CountDownLatch.java
 */

public class CountDownLatch{

/* -------------- AtomicInteger ---------------*/

    /*@
     given int r,l,max;
     requires max >0 && 100%max==0;
     requires grp:inv(part(r,l,max));
     ensures gep:inv(part(r,\result,max));
     */
    int get();

    /*@
     given int r, l, max;
     requires max >0 && x <=max && n<=max && 100%max==0;
     requires (crp:inv(part(r,l,max))) ** crs:inv(part(0,n,max)-part(0,x,max));
     ensures \result ==> (cepp:inv(part(r,n,max))) ** cesp:inv(part(0,x,max)-part(0,n,max));
     ensures !\result ==> (cepn:inv(part(r,l,max))) ** cesn:inv(part(0,n,max)-part(0,x,max));
     */
    boolean compareAndSet(int x,int n);
/* -------------- CoundDownLatch --------------*/

    //@ resource inv(zfrac p)=Perm(data,p);
    /*@ pure zfrac part(int r,int v, int M){ 
            return    (r==2 && v==0 && M>0 && M<=100) ? (100/M):
                    ((r==0 && v>=0 && M>0 && v<=M && M<=100) ? (((M-v)*100)/M) : 0 ); }
    @*/
    //shared resource
    int data;
    // count is actually ghost final!
    //@ int count;

    // role AT = 1;
    //@ requires Value(count) ** count>0 ** count<=100 ** 100%count==0 ** crri:inv(100/count);
    //@ ensures \result ==> Value(count) ** count>0 ** count<=100 ** 100%count==0;
    //@ ensures !\result ==> Value(count) ** count>0 ** count<=100 ** 100%count==0 ** ceri:inv(100/count);
    public boolean tryCountDown(){
    // Decrement count; signal when transition to zero
        boolean r = false;
        //int res = -1;
        //@ int AT=1;

        //@ witness tgrp:inv(*);
        //@ witness tgep:inv(*);
        //@ witness tce:inv(*);
        //@ fold tgrp: inv(part(AT,count,count));

        int c = get() /*@ with{ max=count; r = AT; l = count; grp = tgrp; } then { tgep = gep; } @*/ ;

        if (c > 0){
            //@ assume c<=count;
            int nextc = c-1;
            //@ witness tcesp:inv(*);
            //@ witness tcesn:inv(*);
            //@ witness tcepp:inv(*);
            //@ witness tcepn:inv(*);
            //@ assert ((count-nextc)*100)/count-((count-c)*100)/count == 100/count;
            //@ assert part(AT,c,count)==0;

            r = compareAndSet(c, nextc)/*@ with{ max=count; r=AT; l=c; crp = tgep; crs = crri; } then { tcesp = cesp; tcesn = cesn; tcepp = cepp; tcepn=cepn;} @*/;

        /*@    if(r){
                unfold tcesp: inv(part(0,c,count)-part(0,nextc,count));
                unfold tcepp: inv(part(AT,nextc,count));
            }else{
                ceri = tcesn;
                unfold tcepn: inv(part(AT,c,count));
            }
         @*/
        }
        /*@
         if(c<=0)
            ceri = crri;
         @*/
        return r;
    }

    // role AT = 1;
    //@ requires Value(count) ** count>0 ** count<=100 ** count%100==0 ** crri:inv(100/count);
    //@ ensures Value(count) ** count>0 ** count<=100 ** count%100==0;
    public void countDown(){

        boolean stop = false;
        //@ loop_invariant (!stop) ==> Value(count)**count>0**count<=100 ** count%100==0 **linv:inv(100/count);
        //@ loop_invariant (stop) ==> Value(count)**count>0**count<=100 ** count%100==0;
        while(!stop) /*@ with { linv = crri; } @*/{
            stop = tryCountDown() /*@ with { crri = linv; } then { linv = ceri; } @*/;
        }

    }

    // role PT = 2;
    //@ requires Value(count) ** count>0 ** count<=100 ** count%100==0;
    //@ ensures  Value(count) ** count>0 ** count<=100 ** count%100==0 ** aeri:inv(100/count);
    public void await(){
        //@ int last = count;
        //@ int max = count;
        //@ int PT = 2;
        //@ witness tgrp:inv(*);
        //@ witness tgep:inv(*);
        //@ fold tgrp:inv(part(PT,last,max));
        int s = get() /*@ with{ max = max; r = PT; l=last; grp = tgrp; } then { tgep=gep; } @*/;

        //@ loop_invariant linv:inv(part(PT,s,max));
        while(s!=0) /*@ with { linv = tgep; } then { aeri = linv; } @*/ {
          s = get() /*@ with { max = max; r=PT; l=s; grp=linv; } then { linv=gep; } @*/;
        }
    }  
}
