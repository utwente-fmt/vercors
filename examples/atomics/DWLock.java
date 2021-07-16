// -*- tab-width:4 ; indent-tabs-mode:nil -*-
//:: cases DepositWithdrawLockWitnesses
//:: tools chalice
//:: options --explicit
/*
 
 Example:
    DWLock (stands for Deposit-Withdraw Lock)
 Description:
    How to implement a single-entrant lock using AtomicInteger? 
    A single-entrant lock which uses deposit-withdraw approach of AtomicInteger specification.
 
  The command line to verify with the VerCors Tool is:
  
  vct --chalice --explicit DWLock.java
  
  The expected result is Pass.

 Author:
    Afshin Amighi
 Status:
    Pass.
 */

class DWLock {

    // LOCK = 1 , UNLOCK = 0
    /*@
        public resource resource_pred(int x) = true;
        public resource value(int x) = x==0 ==> Perm(data,100);
     
        public resource resource_invariant() = Perm(data,100);
     */

    //@ resource state(int x) = true;


    private int data;

    DWLock() {
    }

    /*@
     requires (set_rr:resource_pred(n)) ** set_rv:value(n);
     ensures set_ev:value(n);
     */
    void set(int n);

    /*@
     requires  true;
     ensures ((\result ==> eql_evp:value(n) ) ** (!\result ==> true)) ;
     */
    boolean equal(int n);

    /*@
     requires    (cas_rr:resource_pred(n)) ** cas_rv:value(n);
     ensures    (\result ==> (cas_erp:resource_pred(o)) ** cas_evp:value(o) ) **
     (!\result ==> (cas_ern:resource_pred(n)) ** cas_evn:value(n));
     */
    boolean cas(int o, int n);

    /*@
     requires true;
     ensures lck_eri:resource_invariant();
     */
    void dolock() {

        boolean succ = false;
        
        /*@
            witness vtmp1:value(*);
            witness rtmp1:resource_pred(*);
            witness ri:resource_invariant();
         
            fold vtmp1:value(1);
            fold rtmp1:resource_pred(1);
         */
        //@ loop_invariant !succ ==> ((invv:value(1)) ** invr:resource_pred(1)) ;
        //@ loop_invariant succ ==> ((vtmp2:value(0)) ** rtmp2:resource_pred(0)) ;
        while (!succ) /*@ with { invv = vtmp1; invr = rtmp1; } then { unfold vtmp2:value(0); } */ {
            succ = (cas(0, 1) /*@ with { cas_rr = invr; cas_rv = invv; } then { invv = cas_evn; invr = cas_ern; rtmp2 = cas_erp; vtmp2 = cas_evp; } */);
        }
        //@ fold ri:resource_invariant();
        //@ lck_eri = ri;
    }

    /*@
     requires ulk_rri:resource_invariant();
     ensures true;
     */
    void dounlock() {
        
        /*@
            witness vtmp1:value(*);
            witness rtmp1:resource_pred(*);
         */
        //@ unfold ulk_rri:resource_invariant();
        //@ fold rtmp1:resource_pred(0);
        //@ fold vtmp1:value(0);
        this.set(0) /*@ with{ set_rr=rtmp1; set_rv=vtmp1; } */;

    }


    /*@
     requires true;
     ensures true;
     */
    void write(int v) {

        //@ witness inv:resource_invariant();
        dolock() /*@ with{} then { inv = lck_eri; } */;
        //@ unfold inv:resource_invariant();
        data = 1;
        //@ fold inv:resource_invariant();
        dounlock() /*@ with { ulk_rri=inv; } */;
    }
}
