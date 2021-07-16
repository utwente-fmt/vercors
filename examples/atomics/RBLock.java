// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases SESLockWitnesses
//:: tools chalice
//:: options --explicit

/**
 * Example: SESLock
 * Description: SESLock is the single-entrant spin lock using AtomicInteger as synchronizer.
 * The contracts for the AtomicInteger is the version without magic-wand (delta).
 * Author: Afshin Amighi
 * Status: Pass.
 * ToDo List:
 * 1. check the contracts for cas wrt l or o?
 * 2. allowed in loop invariant: how to preserve?
 * The command line to verify with the VerCors Tool is:
 * <p>
 * vct --chalice --explicit RBLock.java
 * <p>
 * The expected result is Pass.
 */


class SESLock {


    // roles and states definitions
    //final int S = 0 ,T = 1 ;
    //final int U = 0 , L = 1;

    // abstract predicates for the AtomicInteger

	/*@
	 resource handle(int role,int val);

	 resource allowed(int role, int last, int next) =
	 ( role == 1 && last == 0 && next == 1 ==> true ) **
	 ( role == 1 && last == 1 && next == 0 ==> true );

	 resource assigned(int role, int val) =
	 ( role == 1 && val == 0 ==> true ) **
	 ( role == 1 && val == 1 ==> true ) **
	 ( role == 0 && val == 0 ==> Perm(data,100) ) **
	 ( role == 0 && val == 1 ==> true ) ;

	 */


    // data field of the producer-consumer
    int data;


    Lock() {
    }

    /*@
     given int r, l;
     requires (srh:handle(r,l)) ** (sra:allowed(r,l,x)) ** (srp:assigned(r,l)) ** (srs:assigned(0,x));
     ensures (seh:handle(r,x)) ** (sep:assigned(r,x));
     */
    void set(int x);

    /*@
     given int r,l;
     requires (grh:handle(r,l)) ** (grp:assigned(r,l));
     ensures (geh:handle(r,\result)) ** (gep:assigned(r,\result));
     */
    int get();

    // the last ensures is temp just to solve the cas loop. ToDo: do we need it? if we remove it , how to preserve the loop invariant?
	/*@
	 given int r, l;
	 requires (crh:handle(r,l)) ** (cra:allowed(r,o,x)) ** (crp:assigned(r,l)) ** (crs:assigned(0,x));
	 ensures \result ==> (cehp:handle(r,x)) ** (cepp:assigned(r,x)) ** (cesp:assigned(0,o));
	 ensures !\result ==> (cehn:handle(r,l)) ** (cepn:assigned(r,l)) ** (cesn:assigned(0,x));
	 ensures !\result ==> (cea:allowed(r,o,x));
	 */
    boolean cas(int o, int x);

    /*@

     requires lrh:handle(1,0);
     ensures Perm(data,100);
     ensures leh: handle(1,1);
     */
    void dolock() {
        //@ int last = 0;
        //@ int role = 1, S = 0;

        //@ witness tgrp:assigned(*,*);
        //@ fold tgrp:assigned(role,last);

        //@ witness tgep:assigned(*,*);
        //@ witness tgeh:handle(*,*);

        int curr = get() /*@ with { r=role; l = last; grh = lrh; grp = tgrp; } then { tgeh = geh; tgep = gep; } */;

        boolean succ = false;

        //@ witness tcra:allowed(*,*,*);
        //@ fold tcra:allowed(role,0,1);

        //@ witness tcrs:assigned(*,*);
        //@ witness tces:assigned(*,*);
        //@ witness tcepp:assigned(*,*);
        //@ fold tcrs:assigned(S,1);

        //@ loop_invariant succ ==> (invhp: handle(role,1)) ** (invpp: assigned(role,1)) ** (invsp: assigned(S,0));
        //@ loop_invariant !succ ==> (invhn: handle(role,curr)) ** (invpn: assigned(role,curr)) ** (invsn: assigned(S,1));
        //@ loop_invariant !succ ==> (inva:allowed(role,0,1));
        while (!succ) /*@ with{ invhn = tgeh; invpn = tgep; invpp = tgep;  invsn = tcrs; inva = tcra; }  then { tces = invsp; tcepp = invpp; leh = invhp; } */ {
            succ = cas(0, 1) /*@ with{ r = role; l = curr; crh = invhn; crp = invpn; cra = inva; crs = invsn; }
							 then{ invhn = cehn; invpn = cepn; invsn = cesn; invhp = cehp; invpp = cepp; invsp = cesp;  inva = cea; } */;
        }

        //@ unfold tces: assigned(S,0);
    }

    /*@
     requires (urh:handle(1,1)) ** Perm(data,100);
     ensures ueh: handle(1,0);
     */
    void dounlock() {

        //@ int role = 1, S=0;

        //@ witness tsrp:assigned(*,*);
        //@ witness tsrs:assigned(*,*);
        //@ fold tsrp:assigned(role,1);
        //@ fold tsrs:assigned(S,0);

        //@ witness tsra:allowed(*,*,*);
        //@ fold tsra:allowed(role,1,0);

        set(0) /*@ with{ r = role; l = 1; srh = urh; srp = tsrp; sra = tsra; srs = tsrs; } then { ueh = seh; } */;
    }


}
