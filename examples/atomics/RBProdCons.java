// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases ProdConsWitnesses
//:: tools chalice
//:: options --explicit
/*
 Example: ProdCons
 Description: Single producer and single-consumer verification using AtomicInteger. 
			The contracts for the AtomicInteger is the version without magic-wand (delta).
 Author: Afshin Amighi
 command: vct --chalice --explicit RBProdCons.java
 Status: Pass.
 
*/


class ProdCons {


    // roles and states definitions
    //final int S = 0 ,P = 1 , C = 2;
    //final int E = 0 , F = 1;

    // abstract predicates for the AtomicInteger
	
	/*@
	 resource handle(int role,int val);
	 
	 resource allowed(int role, int last, int next) =
		( role == 1 && last == 0 && next == 1 ==> true ) **
		( role == 2 && last == 1 && next == 0 ==> true );
	 
	 resource assigned(int role, int val) =
		( role == 1 && val == 0 ==> Perm(data,100) ) **
		( role == 2 && val == 1 ==> Perm(data,100) ) **
		( role == 0 ==> true ) **
		( role == 1 && val == 1 ==> true ) **
		( role == 2 && val == 0 ==> true ) ;
	 */


    // data field of the producer-consumer
    int data;

    // methods set and get for the AtomicInteger
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

    /*@
     requires Perm(data,100) ** prh:handle(1,0);
     ensures  peh:handle(1,0);
     */
    void produce() {

        //@ int role = 1, last = 0 , S = 0;
        write();
		
		/*@
		 witness tmp_allowed:allowed(*,*,*);
		 witness tmp_assigned1:assigned(*,*);
		 witness tmp_assigned2:assigned(*,*);
		 witness tmp_assigned3:assigned(*,*);
		 witness tmp_handle1:handle(*,*);
		 witness tmp_handle2:handle(*,*);
		 */
		/*@
		 fold tmp_allowed: allowed(role,last,1);
		 fold tmp_assigned1: assigned(role,last);
		 fold tmp_assigned2: assigned(S,1);
		 */

        set(1) /*@ with { r = role ; l = 0; srh = prh ; sra = tmp_allowed; srp = tmp_assigned1; srs = tmp_assigned2; } then { tmp_handle1 = seh; tmp_assigned3 = sep; }  */;

        int check = 1;

        //@ loop_invariant invh: handle(role,check);
        //@ loop_invariant inva: assigned(role,check);


        while (check != 0) /*@ with { invh = tmp_handle1; inva = tmp_assigned3; } then { peh = invh; } */ {
            check = get() /*@ with { r = role; l = check; grh = invh;  grp = inva; } then { invh = geh; inva = gep; } */;
        }
    }


    /*@
     requires Perm(data,100);
     ensures Perm(data,100);
     */
    void write();


    // do we have to call this methid when the buffer is in particular state? it can be in any state!
	/*@
	 requires crh:handle(2,0);
	 ensures ceh:handle(2,0);
	 */

    void consume() {

        //@ int role = 2;
        //@ int last = 0;
        //@ int S = 0;
		/*@
		 witness tmp_assigned1:assigned(*,*);
		 witness tmp_assigned2:assigned(*,*);
		 witness tmp_assigned3:assigned(*,*);
		 witness tmp_handle:handle(*,*);
		 */
	
		/*@
		 fold tmp_assigned1: assigned(role,last);
		 */
        int check = 0;
        //@ loop_invariant invh: handle(role,check);
        //@ loop_invariant inva: assigned(role,check);
        while (check != 1) /*@ with { invh = crh; inva = tmp_assigned1; } then{ tmp_handle = invh; tmp_assigned3 = inva; } */ {
            check = get() /*@ with { r = role; l = check; grh = invh; grp = inva; } then { invh=geh; inva = gep; } */;
        }

        //@ unfold tmp_assigned3: assigned(role,check);

        read();
		
		/*@
		 witness tsrp:assigned(*,*);
		 witness tsrs:assigned(*,*);
		 witness tsra:allowed(*,*,*);
		 */
		/*@
		 fold tsrp: assigned(role,1);
		 fold tsrs: assigned(S,0);
		 fold tsra: allowed(role,1,0);
		 */

        set(0) /*@ with { r = role; l = check; sra = tsra; srs = tsrs; srh = tmp_handle; srp = tsrp; } then { ceh = seh; } */;

    }

    /*@
     requires Perm(data,100);
     ensures Perm(data,100);
     */
    void read();


}
