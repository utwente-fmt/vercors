
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
    //@ resource trans(int role,int last,int next)=( (role == 1 )  ? true : false );
    //@ ghost pure zfrac part(int r, int v,int M){ return (r == 0 && v == 0) ? 1\1:0; }
    //@ ghost pure zfrac min(zfrac l, zfrac r){ return (l-r<0) ? 0:l-r; }

    // for simplicity we take resource_invariant=Perm(data,p)
    //@ resource inv(zfrac p)= Perm(data,p) ** Perm(count,p);

    private int        count;

    //@ resource state(int id , int hld) = (hld > 0 )==> Perm(count,1\1);

    /*@
     given int r;
     given int l;
     given int max;
     requires (handle(r,l)) ** (trans(r,l,x)) ** (inv(part(r,l,max))) ** (inv(part(0,x,max)));
     ensures (handle(r,x)) ** (inv(part(r,x,max)));
     */
    void zet(int x);

    /*@
     given int r;
     given int l;
     given int max;
     requires (handle(r,l)) ** inv(part(r,l,max));
     ensures (handle(r,\result)) ** inv(part(r,\result,max));
     */
    int get();

    /*@
     given int r; requires r == 1;
     given int l;
     given int max;
     requires n != 0 && x == 0;
     requires (handle(r,l)) ** (trans(r,x,n)) ** (inv(part(r,l,max))) ** (inv(min(part(0,n,max),part(0,x,max))));
     ensures \result ==> (handle(r,n)) ** (inv(part(r,n,max))) ** inv(min(part(0,x,max),part(0,n,max)));
     ensures !\result ==> (handle(r,l)) ** (inv(part(r,l,max))) ** inv(min(part(0,n,max),part(0,x,max)));
     */
    boolean compareAndSet(int x,int n);



    //@ given int last;
    //@ given int hld;
    //@ requires tid > 0;
    //@ requires state(tid,hld);
    //@ requires handle(1,last);
    //@ ensures state(tid,hld+1);
    //@ ensures (hld == 0) ==> Perm(data,1\1);
    //@ ensures  handle(1,tid);
    public void dolock(int tid){
        boolean res = false;

        //@ ghost int role = 1;
        //@ ghost int S=0;
        //@ ghost int M=1;

        /*@ fold inv(part(role,last,M)); */

        int curr = get() /*@ given { max = M, r=role, l = last }*/;
        // check re-entrant

        if ( tid == curr ) {
            //@ assume (hld > 0);
            //@ unfold state(tid,hld);
            //@ assume (hld == count);

            count = count+1;

        }

        // check first-entrant
        if( tid != curr){
            //@ assume (hld == 0);

            boolean succ = false;
            //@ fold inv(min(part(S,tid,M),part(S,0,M)));
            //@ loop_invariant  !succ    ==> (handle(role,curr)) ** ( inv(part(role,curr,M))) **  inv(min(part(S,tid,M),part(S,0,M)));
            //@ loop_invariant  succ    ==>    (handle(role,tid)) ** ( inv(part(role,tid,M))) **  inv(min(part(S,0,M),part(S,tid,M)));
            while (!succ) {

                //@     fold trans(role,0,tid);
                succ = compareAndSet(0,tid) /*@ given { max = 1, r = role, l = curr } @*/ ;
            }
            //@ unfold  inv(min(part(S,0,1),part(S,tid,1)));
            //@ assume (hld == count);
            count = count+1;
        }
        //@ fold state(tid,hld+1);
        return;

    }



    // unlock is only called with a valid tid (tid > 0) and valid hld (hld > 0)
    //@ given int hld;
    //@ requires tid > 0;
    //@ requires hld > 0;
    //@ requires  state(tid,hld);
    //@ requires handle(1,tid);
    //@ requires hld > 0 ==> Perm(data,write);
    //@ ensures hld == 1 ==>  handle(1,0);
    //@ ensures hld > 1 ==> ( handle(1,tid)) ** Perm(data,write);
    //@ ensures state(tid,hld-1);
    public void unlock(int tid){
        //@ ghost int role = 1;
        //@ ghost int S=0;
        //@ ghost int M=1;
        //@ ghost int last = tid;

        //@ fold inv(part(role,last,M));
        int curr = get() /*@ given { max = M, r=role, l=last } @*/;

        // this should be a global invariant
        //@ assume (curr==tid);

        if ( curr == tid) {
            //@ unfold state(tid,hld);
            //@ assume (count == hld);
            if (count == 1) {
                count = count-1;
                //@ fold state(tid,count);
                //@ fold inv(part(role,curr,M));
                //@ fold inv(part(S,0,M));
                //@ fold trans(role,curr,0);
                zet(0) /*@ given{ max = M, r = role, l = curr } @*/;
            }
            else{
                if (count > 1) {
                    count = count-1;
                    //@ fold state(tid,hld-1);
                }
            }
        }
    }

}
