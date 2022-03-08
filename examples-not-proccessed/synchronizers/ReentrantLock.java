// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases ReentLock
//:: tools chalice
//:: verdict Pass
//:: options --explicit
 /*
 Example: ReentLock
 Description: ReentLock is the re-entrant lock using AtomicInteger as synchronizer.
 Author: Afshin Amighi
 Status: Pass.
 command: vct --chalice --explicit ReentrantLock.java
 */

public class ReentLock{

    //shared resource
    private int data;

    //  int SynchronizerRole (S) = 0, ThreadRole = 1;
    //    int UNLOCKED = 0 , LOCKED = threadid;

    //@ resource handle(int role,int val);
    //@ resource trans(int role,int last,int next)=( role == 1  ==> true );
    //@ pure zfrac part(int r, int v,int M){ return (r == 0 && v == 0) ? 100:0; }
    // for simplicity we take resource_invariant=Perm(data,p)
    //@ resource inv(zfrac p)= Perm(data,p) ** Perm(count,p);

    private int        count;

    //@ resource state(int id , int held) = (held > 0 )==> Perm(count,100);

    /*@
     given int r, l, max;
     requires (srh:handle(r,l)) ** (sra:trans(r,l,x)) ** (srp:inv(part(r,l,max))) ** (srs:inv(part(0,x,max)));
     ensures (seh:handle(r,x)) ** (sep:inv(part(r,x,max)));
     */
    void set(int x);

    /*@
     given int r,l,max;
     requires (grh:handle(r,l)) ** grp:inv(part(r,l,max));
     ensures (geh:handle(r,\result)) ** gep:inv(part(r,\result,max));
     */
    int get();

    /*@
     given int r, l, max;
     requires (crh:handle(r,l)) ** (cra:trans(r,x,n)) ** (crp:inv(part(r,l,max))) ** (crs:inv(part(0,n,max)-part(0,x,max)));
     ensures \result ==> (cehp:handle(r,n)) ** (cepp:inv(part(r,n,max))) ** cesp:inv(part(0,x,max)-part(0,n,max));
     ensures !\result ==> (cehn:handle(r,l)) ** (cepn:inv(part(r,l,max))) ** cesn:inv(part(0,n,max)-part(0,x,max));
     */
    boolean compareAndSet(int x,int n);

    /*
     requires Perm(owner,100) * Perm(holds,100) * P ; where P is what is needed for lock constructor
     ensures Perm(owner,100) * Perm(holds,100) * P * (\forall* handles(T,UNLOCKED));
     */
    /*ReentLock(){

        state = new AtomicInteger(UNLOCKED);
        // handles for all the threads
        owner = null;
        holds = 0;

    }*/

    //@ given int last;
    //@ given int held;
    //@ requires tid > 0;
    //@ requires lrs: state(tid,held);
    //@ requires lrh: handle(1,last);
    //@ ensures les: state(tid,held+1);
    //@ ensures (held == 0) ==> Perm(data,100) ;
    //@ ensures leh: handle(1,tid);
    public void dolock(int tid){
        boolean res = false;

        //@ int role = 1, S=0, M=1;

        /*@ witness tgrp:inv(*);
            fold tgrp:inv(part(role,last,M));

            witness tgep:inv(*);
            witness tgeh:handle(*,*);
         */

        int curr = get() /*@ with { max = M; r=role; l = last; grh = lrh;  grp = tgrp; } then { tgeh = geh; tgep = gep; } */;
        // check re-entrant
        if ( tid == curr ) {
            //@ assume (held > 0);
            //@ unfold lrs:state(tid,held);
            //@ assume (held == count);

            count = count+1;

            //@ leh=tgeh;
        }
        // check first-entrant
        if( tid != curr){
            //@ assume (held == 0);

            boolean succ = false;
            //@ witness tcra:trans(*,*,*);
            //@ witness tcrs:inv(*);
            //@ witness tces:inv(*);
            //@ witness tcepp:inv(*);
            //@ fold tcrs:inv(part(S,tid,M)-part(S,0,M));
            //@ loop_invariant  !succ    ==> (invhn:handle(role,curr)) ** (invpn: inv(part(role,curr,M))) ** invsn: inv(part(S,tid,M)-part(S,0,M));
            //@ loop_invariant  succ    ==>    (invhp:handle(role,tid)) ** (invpp: inv(part(role,tid,M))) ** invsp: inv(part(S,0,M)-part(S,tid,M));
            while (!succ) /*@ with{ invhn = tgeh; invpn = tgep; invpp = tgep;  invsn = tcrs; }
                           then { tces = invsp; tcepp = invpp; leh = invhp;   } @*/ {

               //@     fold tcra:trans(role,0,tid);
                succ = compareAndSet(0,tid) /*@ with{ max = 1; r = role; l = curr; crh = invhn; crp = invpn; cra = tcra; crs = invsn; }
                                        then{ invhn = cehn; invpn = cepn; invsn = cesn; invhp = cehp; invpp = cepp; invsp = cesp;  } @*/ ;
            }
            //@ unfold tces: inv(part(S,0,1)-part(S,tid,1));
            //@ assume (held == count);
            count = count+1;
        }
        //@ fold les:state(tid,held+1);
        return;
    }

    // unlock is only called with a valid tid (tid > 0) and valid held (held > 0)
    //@ given int held;
    //@ requires tid > 0;
    //@ requires held > 0;
    //@ requires urs: state(tid,held);
    //@ requires urh:handle(1,tid);
    //@ requires held > 0 ==> Perm(data,100);
    //@ ensures held == 1 ==> ueuh: handle(1,0);
    //@ ensures held > 1 ==> (uelh: handle(1,tid)) ** Perm(data,100);
    //@ ensures ues:state(tid,held-1);
    public void unlock(int tid){
        //@ int role = 1, S=0, M=1;
        //@ int last = tid;

        //@ witness tgrp:inv(*);
        //@ fold tgrp:inv(part(role,last,M));
        //@ witness tgep:inv(*);
        //@ witness tgeh:handle(*,*);
        int curr = get() /*@ with { max = M; r=role; l=last; grh=urh;  grp=tgrp; } then { tgeh=geh; tgep=gep; } @*/;

        // this should be a global invariant
        //@ assume( curr==tid );

        if ( curr == tid) {

            //@ unfold urs:state(tid,held);
            //@ assume (count == held);
            if (count == 1) {
                count = count-1;
                //@ fold ues:state(tid,count);
                //@ witness tsrp:inv(*);
                //@ witness tsrs:inv(*);
                //@ fold tsrp:inv(part(role,curr,M));
                //@ fold tsrs:inv(part(S,0,M));
                //@ witness tsra:trans(*,*,*);
                //@ fold tsra:trans(role,curr,0);
                set(0) /*@ with{ max = M; r = role; l = curr; srh = tgeh; srp = tsrp; sra = tsra; srs = tsrs; } then { ueuh = seh; } @*/;
            }
            else{
                if (count > 1) {
                    count = count-1;
                    //@ fold ues:state(tid,held-1);
                    //@ uelh = tgeh;
                }
            }
        }
    }
}
