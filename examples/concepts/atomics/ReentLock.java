
public class ReentLock {

    //shared resource
    private int data;

    //  int SynchronizerRole = 0, ThreadRole = 1;
    //	int UNLOCKED = 0 , LOCKED = threadid;

	/*@
	 resource handle(int role,int val);
	 resource allowed(int role,int last,int next)=( role == 1  ==> true );
	 resource assigned(int role, int val) = (role == 0 && val == 0) ==> (Perm(data,1\1) ** Perm(count,1\1));
	 */

    private int count;

	/*@
	 resource state(int id , int hld) = (hld > 0) ==> Perm(count,1\1);
	 */


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
     requires (handle(r,l)) ** assigned(r,l);
     ensures (handle(r,\result)) ** assigned(r,\result);
     */
    int get();


    // the last ensures is temp just to solve the cas loop.  do we need it? if we remove it , how to preserve the loop invariant?
	/*@
     given int r;
     given int l;
	 requires (handle(r,l)) ** (allowed(r,o,x)) ** (assigned(r,l)) ** (assigned(0,x));
	 ensures \result ==> (handle(r,x)) ** (assigned(r,x)) ** (assigned(0,o));
	 ensures !\result ==> (handle(r,l)) ** (assigned(r,l)) ** (assigned(0,x));
	 ensures !\result ==> allowed(r,o,x);
	 */
    boolean compareAndSet(int o, int x);







    /*@
	 given int last;
	 given int hld;
	 requires tid > 0;
	 requires  state(tid,hld);
	 requires  handle(1,last);
	 ensures  state(tid,hld+1);
	 ensures (hld == 0) ==> Perm(data,write) ;
	 ensures  handle(1,tid);
	 */
    public void dolock(int tid){
        boolean res = false;

        //@ ghost int role = 1, S=0;

		/*@ fold assigned(role,last); */

        int curr = get() /*@ given { r=role, l = last }*/;
        // check re-entrant
        if ( tid == curr ) {
            //@ assume (hld > 0);
            //@ assert (hld > 0);
            //@ unfold  state(tid,hld);
            //@ assert (Perm(count,write));
            //@ assume (hld == count);

            count = count+1;
        }
        // check first-entrant
        if( tid != curr){
            //@ assume (hld == 0);
            //@ assert (hld == 0);

            boolean succ = false;
            //@ ghost int next = tid;

			/*@ fold allowed(role,0,next);
			 fold assigned(S,next); */
            //@ loop_invariant  !succ	==> (handle(role,curr)) ** ( assigned(role,curr)) **  assigned(S,next);
            //@ loop_invariant  !succ	==>	allowed(role,0,next);
            //@ loop_invariant  succ	==>	(handle(role,next)) ** ( assigned(role,next)) **  assigned(S,0);
            while (!succ) {
                succ = compareAndSet(0,tid) /*@ given { r = role, l = curr } */ ;
            }
            //@ unfold  assigned(S,0);
            //@ assert (Perm(count,write) ** Perm(data,write));
            //@ assume (hld == count);
            count = count+1;
            //@ assert (Perm(data,write));
        }

        //@ assert Perm(count,write);
        //@ fold state(tid,hld+1);

        return;
    }






    // unlock is only called with a valid tid (tid > 0) and valid hld (hld > 0)
	/*@
	 given int hld;

	 requires tid > 0;
	 requires hld > 0;

	 requires  state(tid,hld);
	 requires handle(1,tid);
	 requires hld > 0 ==> Perm(data,write);

	 ensures hld == 1 ==> ( handle(1,0));
	 ensures hld > 1 ==> ( handle(1,tid)) ** Perm(data,write);
	 ensures state(tid,hld-1);
	 */
    public void unlock(int tid){

        //@ ghost int role = 1, S=0;
        //@ ghost int last = tid;

		/*@ fold assigned(role,last); */

        int curr = get() /*@ given { r=role, l = last } */;

        // this should be a global invariant
        //@ assume( curr == tid );

        if ( curr == tid) {
            //@ assume (hld > 0);
            //@ unfold state(tid,hld);
            //@ assume (count == hld);

            if (count == 1) {
                //@ assert (hld == 1);

                count = count-1;
                //@ assert (count ==0);
                //@ fold state(tid,count);

                //@ fold assigned(role,curr);
                //@ fold assigned(S,0);

                //@ fold allowed(role,curr,0);

                zet(0) /*@ given { r = role, l = curr } */;
            }
            else{
                if (count > 1) {
                    //@ assert (hld > 1);
                    count = count-1;
                    //@ fold state(tid,hld-1);
                }
            }
        }
    }

}