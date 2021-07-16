// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases RBSingleCellWitnesses
//:: suite medium
//:: tools chalice
//:: options --explicit
/*
 Example: SingleCell
 Description: SingleCell is the simplified version of the single method lock-less hash-table using AtomicInteger as synchronizer. 
 The contracts for the AtomicInteger is the version without magic-wand (delta).
 Author: Afshin Amighi
 Command: vct --chalice --explicit RBSingleCell.java
 Status: Pass.
 ToDo List:
 1. check the cas wrt l or o?
 */


class SingleCell {


    // roles and states definitions
    //final int S = 0 ,T = 1 ;
    //final int E = 0 , W = 1 , D = 2;

    // abstract predicates for the AtomicInteger
	
	/*@
	 resource handle(int role,int val) = true;
	 
	 resource allowed(int role, int last, int next) =
	 ( role == 1 && last == 0 && next == 1 ==> true ) **
	 ( role == 1 && last == 1 && next == 2 ==> true );
	 
	 resource assigned(int role, int val) =
	 ( role == 1 && val == 0 ==> true ) **
	 ( role == 1 && val == 1 ==> true ) **
	 ( role == 1 && val == 2 ==> Value(data) ) **
	 ( role == 0 && val == 0 ==> Perm(data,100) ) **
	 ( role == 0 && val == 1 ==> true ) **
	 ( role == 0 && val == 2 ==> true );
	 */
	
	/*@
	 resource contains(int x) = true;
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
     given int r, l;
     requires (crh:handle(r,l)) ** (cra:allowed(r,o,x)) ** (crp:assigned(r,l)) ** (crs:assigned(0,x));
     ensures \result ==> (cehp:handle(r,x)) ** (cepp:assigned(r,x)) ** (cesp:assigned(0,o));
     ensures !\result ==> (cehn:handle(r,l)) ** (cepn:assigned(r,l)) ** (cesn:assigned(0,x));
     */
    boolean cas(int o, int x);

    /*@

     requires foprh:handle(1,0);
     ensures \result == 0 ==> fopecp:contains(v) ;
     ensures \result == 1 ==> fopecf:contains(v) ;
     ensures \result != -2 ==> fopeh: handle(1,2);

     */
    int find_or_put(int v) {

        //@ int role = 1 , S = 0;
        //@ int last = 0;

        // how to provide handle here?
		
		/*@
		  witness tcrh:handle(*,*);
		  witness tcehp:handle(*,*);
		  witness tcehn:handle(*,*);
		  witness tcrs:assigned(*,*);
		  witness tcepp:assigned(*,*);
		  witness tcesp:assigned(*,*);
		  witness tcepn:assigned(*,*);
		  witness tcrp:assigned(*,*);
		  witness tcra:allowed(*,*,*);
		 */

        // what should be the last for handle here? it can be any ...

        // fold tcrh:handle(role,0);
        //@ fold tcra:allowed(role,0,1);
        // fold tcrp:assigned(role,0);
        //@ fold tcrs:assigned(S,1);

        //@ witness tgrp:assigned(*,*);
        //@ fold tgrp:assigned(role,last);

        int curr = get() /*@ with{ r = role ; l = last; grh = foprh; grp = tgrp; } then { tcrh=geh; tcrp = gep; } */;

        boolean b = cas(0, 1) /*@ with { r = role; l = curr; crh = tcrh; cra = tcra; crp = tcrp; crs = tcrs; }
							  then { tcehp=cehp; tcepp= cepp; tcesp = cesp; tcehn = cehn; tcepn = cepn; } */;

        if (b) {

            //@ unfold tcepp:assigned(role,1);
            //@ unfold tcesp:assigned(S,0);

            data = v;


            //@ witness  tsra:allowed(*,*,*);
            //@ fold tsra:allowed(role,1,2);

            //@ fold tcepp: assigned(role,1);
            //@ fold tcesp: assigned(S,2);


            set(2) /*@ with { r = role; l = 1; srh = tcehp; sra = tsra; srp = tcepp; srs = tcesp; } then { fopeh = seh;  } */;
            //@ fold fopecp: contains(v);


            return 0; // PUT
        }
        if (!b) {

            //@ witness tgeh:handle(*,*);
            //@ witness tgep:assigned(*,*);
            //@ witness tlep:assigned(*,*);

            int check = get() /*@ with { r = role; l = curr; grh = tcehn; grp = tcepn; } then { tgep = gep; tgeh = geh; } */;

            //@ loop_invariant invh: handle(role,check);
            //@ loop_invariant invp: assigned(role,check);

            while (check == 1) /*@ with { invh = tgeh ; invp = tgep ; } then { fopeh = invh; tlep = invp; } */ {
                check = get() /*@ with { r = role; l = check; grh = invh; grp = invp; } then { invh = geh; invp = gep; } */;
            }

            if (check == 2) {
                //@ unfold tlep:assigned(role,2);
                if (data == v) {

                    //@ fold fopecf:contains(v);
                    return 1; // FOUND
                }
                return -1; // COLL
            }
        }

        return -2; // ERROR
    }

}
