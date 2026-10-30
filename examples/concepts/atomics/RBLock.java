/**
 Example: SESLock
 Description: SESLock is the single-entrant spin lock using AtomicInteger as synchronizer.
 The contracts for the AtomicInteger is the version without magic-wand (delta).
 Original Author: Afshin Amighi
 Status: Pass.
 ToDo List:
 1. check the contracts for cas wrt l or o?
 2. allowed in loop invariant: how to preserve?
 */


class SESLock{

    // roles and states definitions
    // final int S = 0 ,T = 1 ;
    // final int U = 0 , L = 1;

    // abstract predicates for the AtomicInteger

	/*@
         resource handle(int role,int val);

         resource allowed(int role, int last, int next) =
         ( role == 1 && last == 0 && next == 1 ==> true ) **
         ( role == 1 && last == 1 && next == 0 ==> true );

         resource assigned(int role, int val) =
         ( role == 1 && val == 0 ==> true ) **
         ( role == 1 && val == 1 ==> true ) **
         ( role == 0 && val == 0 ==> Perm(data,1) ) **
         ( role == 0 && val == 1 ==> true ) ;
	 */


    /*@
        given int r;
        given int l;
        requires (handle(r,l)) ** (allowed(r,l,x)) ** (assigned(r,l)) ** (assigned(0,x));
        ensures (handle(r,x)) ** (assigned(r,x));
     */
    void set(int x);


    /*@
     given int r;
     given int l;
     requires (handle(r,l)) ** (assigned(r,l));
     ensures (handle(r,\result)) ** (assigned(r,\result));
     */
    int get();


    // the last ensures is temp just to solve the cas loop. ToDo: do we need it? if we remove it , how to preserve the loop invariant?
	/*@
	    given int r;
	    given int l;
	    requires (handle(r,l)) ** (allowed(r,o,x)) ** (assigned(r,l)) ** (assigned(0,x));
	    ensures \result ==> (handle(r,x)) ** (assigned(r,x)) ** (assigned(0,o));
	    ensures !\result ==> (handle(r,l)) ** (assigned(r,l)) ** (assigned(0,x));
	    ensures !\result ==> (allowed(r,o,x));
    */
    boolean cas(int o,int x);

    // data field of the producer-consumer
    int data;


    Lock(){	}

    /*@

     requires handle(1,0);
     ensures Perm(data,1);
     ensures handle(1,1);
     */
    void dolock(){
        //@ ghost int last = 0;
        //@ ghost int role = 1;
        //@ ghost int S = 0;

        //@ fold assigned(role,last);


        int curr = get() /*@ given { r=role,  l = last } */;

        boolean succ = false;

        //@ fold allowed(role,0,1);
        //@ fold assigned(S,1);

        //@ loop_invariant succ ==> (handle(role,1)) ** (assigned(role,1)) ** (assigned(S,0));
        //@ loop_invariant !succ ==> (handle(role,curr)) ** (assigned(role,curr)) ** (assigned(S,1));
        //@ loop_invariant !succ ==> (allowed(role,0,1));
        while (!succ) {
            succ = cas(0,1) /*@ given { r = role, l = curr } */ ;
        }

        //@ unfold assigned(S,0);
    }

    /*@
	 requires (handle(1,1)) ** Perm(data,1);
	 ensures  handle(1,0);
	 */
    void dounlock(){

        //@ ghost int role = 1;
        //@ ghost int S=0;

        //@ fold assigned(role,1);
        //@ fold assigned(S,0);
        //@ fold allowed(role,1,0);

        set(0) /*@ given { r = role , l = 1} */;
    }
}
