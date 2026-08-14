/*
 Example: CountDownLatch
 Description: CountDownLatch using AtomicInteger as synchronizer.
  The specification is a special case of the contracts provided in the FMCAD'13 paper: p==1/count.
 Author: Afshin Amighi
 Status: Pass.
 */

public class CountDownLatch{

    /* -------------- AtomicInteger ---------------*/

    /*@
     given int r;
     given int l;
     given int max;
     requires max >0 && 100%max==0;
     requires inv(part(r,l,max));
     ensures inv(part(r,\result,max));
     */
    int get();

    /*@
         given int r;
         given int l;
         given int max;
         requires max >0 && x <=max && n<=max && 100%max==0;
         requires (inv(part(r,l,max))) ** inv(part(0,n,max)-part(0,x,max));
         ensures \result ==> (inv(part(r,n,max)) ** inv(part(0,x,max)-part(0,n,max)));
         ensures !\result ==> (inv(part(r,l,max)) ** inv(part(0,n,max)-part(0,x,max)));
     */
    boolean compareAndSet(int x,int n);
    /* -------------- CoundDownLatch --------------*/

    //@ resource inv(int p)= 0<=p ** p<=100 ** Perm(data,p\100);

    /*@ ghost pure int part(int r,int v, int M){
            return    (r==2 && v==0 && M>0 && M<=100) ? (100/M):
                    ((r==0 && v>=0 && M>0 && v<=M && M<=100) ? (((M-v)*100)/M) : 0 ); }
    @*/
    //shared resource
    int data;
    // count is actually ghost final!
    //@ ghost int count;

    // role PT = 2;
    //@ requires Value(count) ** count>0 ** count<=100 ** count%100==0;
    //@ ensures  Value(count) ** count>0 ** count<=100 ** count%100==0 ** inv(100/count);
    public void await(){
        //@ ghost int last = count;
        //@ ghost int max = count;
        //@ ghost int PT = 2;
        //@ fold inv(part(PT,last,max));
        int s = get() /*@ given { max = max, r = PT, l=last } @*/;

        //@ loop_invariant inv(part(PT,s,max));
        while(s!=0) {
            s = get() /*@ given { max = max, r=PT, l=s }@*/;
        }
    }

    // role AT = 1;
    //@ requires Value(count) ** count>0 ** count<=100 ** 100%count==0 ** inv(100/count);
    //@ ensures \result ==> Value(count) ** count>0 ** count<=100 ** 100%count==0;
    //@ ensures !\result ==> Value(count) ** count>0 ** count<=100 ** 100%count==0 ** inv(100/count);
    public boolean tryCountDown(){
        // Decrement count; signal when transition to zero
        boolean r = false;
        //int res = -1;
        //@ ghost int AT=1;

        //@ fold inv(part(AT,count,count));

        int c = get() /*@ given { max=count, r = AT, l = count } @*/ ;

        if (c > 0){
            //@ assume c<=count;
            int nextc = c-1;
            //@ assert ((count-nextc)*100)/count-((count-c)*100)/count == 100/count;
            //@ assert part(AT,c,count)==0;

            r = compareAndSet(c, nextc)/*@ given { max=count, r=AT, l=c }@*/;

        /*@ ghost if(r){
                unfold inv(part(0,c,count)-part(0,nextc,count));
                unfold inv(part(AT,nextc,count));
            }else{
                unfold inv(part(AT,c,count));
            }
         @*/
        }
        return r;
    }

    // role AT = 1;
    //@ requires Value(count) ** count>0 ** count<=100 ** count%100==0 ** inv(100/count);
    //@ ensures Value(count) ** count>0 ** count<=100 ** count%100==0;
    public void countDown(){

        boolean stop = false;
        //@ loop_invariant (!stop) ==> Value(count)**count>0**count<=100 ** count%100==0 **inv(100/count);
        //@ loop_invariant (stop) ==> Value(count)**count>0**count<=100 ** count%100==0;
        while(!stop) {
            stop = tryCountDown();
        }

    }

}