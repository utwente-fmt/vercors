/*
 Example: ProdCons
 Description: Single producer and single-consumer verification using AtomicInteger.
			The contracts for the AtomicInteger is the version without magic-wand (delta).
 Original Author: Afshin Amighi
*/


class ProdCons{
    // roles and states definitions
    //final int S = 0 ,P = 1 , C = 2;
    //final int E = 0 , F = 1;

	/*@
	 resource handle(int role,int val);

	 resource allowed(int role, int last, int next) =
		( role == 1 && last == 0 && next == 1 ==> true ) **
		( role == 2 && last == 1 && next == 0 ==> true );

	 resource assigned(int role, int val) =
		( role == 1 && val == 0 ==> Perm(data,1) ) **
		( role == 2 && val == 1 ==> Perm(data,1) ) **
		( role == 0 ==> true ) **
		( role == 1 && val == 1 ==> true ) **
		( role == 2 && val == 0 ==> true ) ;
	 */


    // methods set and get for the AtomicInteger
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


    // data field of the producer-consumer
    int data;

    /*@
     requires Perm(data,1) ** handle(1,0);
     ensures  handle(1,0);
     */
    void produce(){

        //@ ghost int role = 1;
        //@ ghost int last = 0;
        //@ ghost int S = 0;
        write();

		/*@
		 fold allowed(role,last,1);
		 fold assigned(role,last);
		 fold assigned(S,1);
		 */

        set(1) /*@ given { r = role,  l = 0 }  */;

        int check = 1;

        //@ loop_invariant handle(role,check);
        //@ loop_invariant assigned(role,check);
        while(check != 0) {
            check = get() /*@ given { r = role, l = check }*/;
        }
    }

    /*@
     requires Perm(data,1);
     ensures Perm(data,1);
     */
    void write();

    /*@
     requires Perm(data,1);
     ensures Perm(data,1);
     */
    void read();


    // do we have to call this methid when the buffer is in particular state? it can be in any state!
	/*@
	 requires handle(2,0);
	 ensures handle(2,0);
	 */

    void consume(){

        //@ ghost int role = 2;
        //@ ghost int last = 0;
        //@ ghost  int S = 0;

		/*@
		 fold assigned(role,last);
		 */
        int check = 0;
        //@ loop_invariant handle(role,check);
        //@ loop_invariant assigned(role,check);
        while(check != 1) {
            check = get() /*@ given { r = role, l = check } */;
        }

        //@ unfold assigned(role,check);

        read() ;


		/*@
		 fold assigned(role,1);
		 fold assigned(S,0);
		 fold allowed(role,1,0);
		 */

        set(0) /*@ given { r = role, l = check } */;

    }

}
