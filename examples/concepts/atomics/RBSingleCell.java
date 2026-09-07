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

class SingleCell{
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

    // methods zet and get for the AtomicInteger
	/*@
	 given int r;
	 given int l;
	 requires (handle(r,l)) ** (allowed(r,l,x)) ** (assigned(r,l)) ** (assigned(0,x));
	 ensures (handle(r,x)) ** (assigned(r,x));
	 */
    void zet(int x);


    /*@
	 given int r;
	 given int l;
	 requires (handle(r,l)) ** (assigned(r,l));
     ensures (handle(r,\result)) ** (assigned(r,\result));
     */
    int get();


    /*@
	 given int r;
	 given int l;
     requires (handle(r,l)) ** (allowed(r,o,x)) ** (assigned(r,l)) ** (assigned(0,x));
     ensures \result ==> (handle(r,x)) ** (assigned(r,x)) ** (assigned(0,o));
     ensures !\result ==> (handle(r,l)) ** (assigned(r,l)) ** (assigned(0,x));
     */
    boolean cas(int o,int x);

    // data field of the producer-consumer
    int data;






    /*@
     requires handle(1,0);
     ensures \result == 0 ==> contains(v) ;
     ensures \result == 1 ==> contains(v) ;
     ensures \result != -2 ==>  handle(1,2);
     */
    int find_or_put(int v){
        //@ ghost int role = 1 , S = 0;
        //@ ghost int last = 0;

        // how to provide handle here?

        // what should be the last for handle here? it can be any ...

        //@ fold allowed(role,0,1);
        //@ fold assigned(S,1);

        //@ fold assigned(role,last);

        int curr = get() /*@ given { r = role , l = last } */;

        boolean b = cas(0,1) /*@ given { r = role, l = curr } */;

        if (b) {

            //@ unfold assigned(role,1);
            //@ unfold assigned(S,0);

            data = v;


            //@ fold allowed(role,1,2);

            //@ fold  assigned(role,1);
            //@ fold  assigned(S,2);


            zet(2) /*@ given { r = role, l = 1} */;
            //@ fold contains(v);


            return 0; // PUT
        }
        if (!b) {
            int check = get() /*@ given { r = role, l = curr } */;

            //@ loop_invariant  handle(role,check);
            //@ loop_invariant  assigned(role,check);

            while(check ==1) {
                check = get() /*@ given { r = role, l = check } */;
            }

            if (check == 2) {
                //@ unfold assigned(role,2);
                if(data == v){

                    //@ fold contains(v);
                    return 1; // FOUND
                }
                return -1; // COLL
            }
        }

        return -2; // ERROR
    }




}
