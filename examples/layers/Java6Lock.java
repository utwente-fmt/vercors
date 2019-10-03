// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases Java6LockImplementation
//:: suite problem-fail
//:: tools silicon

/*
    vct --silver=silicon Java6Lock.java
 */
//begin(all)
class AtomicInteger {

  int dummy;

  int val;
  
  int get(){
    return val;
  }
  
  void set(int v){
    val=v;
  }
  
  boolean cas(int e,int v){
    if(val==e){
      val=v;
      return true;
    } else {
      return false;
    }
  }

}

class Subject {

  /*@
    resource inv();
  @*/

}


class Thread {

  Thread(){
    //@ assume false;
  }

  int T;
  
  int L;

  Lock locks[];

  /*@
    inline resource APerm(loc<AtomicInteger> ref,frac p)=
      Value(ref)**Perm(ref.val,p);
    inline resource APointsTo(loc<AtomicInteger> ref,frac p,int v)=
      Value(ref)**PointsTo(ref.val,p,v);
    
    inline thread_local resource common()= Value(T) **
      Value(L) ** L > 0 ** 0 <= \current_thread < T **
      Value(locks) ** locks != null **
      (\forall* int l ; 0 <= l < L ;
        Value(locks[l]) ** Value(locks[l].count) **
        Value(locks[l].owner) ** Value(locks[l].subject)
        ** Value(locks[l].T) ** locks[l].T==T);
        
// begin(lockset)
thread_local resource lockset(bag<int> S)=
  Value(T) ** 0 <= \current_thread < T **
  Value(L) ** L > 0 ** Value(locks) ** locks != null **
  (\forall* int l ; 0 <= l < L ;
     Value(locks[l]) ** Value(locks[l].subject) **
     Value(locks[l].T) ** locks[l].T==T **
     Value(locks[l].held) ** locks[l].lockset_part() **
     Value(locks[l].count) ** Value(locks[l].count.dummy) ** // skip(lockset) silicon incompleteness
     Value(locks[l].owner) ** Value(locks[l].owner.dummy) ** // skip(lockset) silicon incompleteness
     locks[l].held[\current_thread]==(l \memberof S)
  );
    // end(lockset)
  @*/
  
  /*@
    given bag<int> S;
    requires common();
    requires 0 <= lock_id < L;
    requires lockset(S);
    ensures  common();
    ensures  0 <= lock_id < L;
    ensures  \result ==> lockset(S+bag<int>{lock_id});
    ensures  \result && (lock_id \memberof S)==0 ==>
                locks[lock_id].subject.inv();
    ensures  (lock_id \memberof S)>0 ==> \result;
    ensures  !\result ==> lockset(S);
  @*/
  boolean trylock(int lock_id){
    boolean res;
    //@ unfold lockset(S);
    res=locks[lock_id].trylock();
    /*@
      if (res) {
        fold lockset(S+bag<int>{lock_id});
      } else {
        fold lockset(S);
      }
    @*/
    return res;
  }
}



class Lock {

  /*@ 
    int T; // Maximum number of threads.

    int held[];
  
    int holder;
    
    Subject subject;
  @*/
  
  AtomicInteger count;
  
  AtomicInteger owner;

  /*@
    inline resource common()=
      Value(count) ** Value(owner) **
      Value(T) ** 0 <= \current_thread < T **
      Value(held) ** held != null **
      Value(subject);
      
    inline resource APerm(loc<AtomicInteger> ref,frac p)=
      Value(ref)**Perm(ref.val,p);
    inline resource APointsTo(loc<AtomicInteger> ref,frac p,int v)=
      Value(ref)**PointsTo(ref.val,p,v);
  @*/
  
/*@
// begin(context_everywhere)
resource csl_invariant()= Value(T) ** T > 0 **
   Value(held) ** held != null ** Value(subject) **
   APerm(count,1f/2) ** APerm(owner,1f/2) **
   (count.val == 0 ==> subject.inv() **
           APerm(count,1f/2) ** APerm(owner,1f/2)) **
   Perm(holder,1) ** -1 <= holder < T **
   (holder == -1) == (count.val == 0) **
   (\forall* int i; 0 <= i < T ;
     Perm(held[i],1f/2) ** (i!=holder ==> held[i]==0)
     ** held[i] >= 0 ** (held[i]==0 ==> owner.val!=i)
   );
// end(context_everywhere)
 @*/

  /*@
    inline thread_local
// begin(context_everywhere)
    resource lockset_part()= held!=null **
       Perm(held[\current_thread],1f/2) **
       (held[\current_thread] > 0 ==>
          APointsTo(count,1f/2,held[\current_thread]) **
          APointsTo(owner,1f/2,\current_thread));
// end(context_everywhere)
  @*/


  /*@
    ensures \result==\current_thread;
  @*/
  public /*@ thread_local pure @*/ static int currentThread();

// begin(tryacquire)
  /*@
  requires common() ** lockset_part();
  ensures  common() ** lockset_part() ** (!\result ==>
    held[\current_thread] == \old(held[\current_thread]))
  ** (\result ==> held[\current_thread] ==
                          \old(held[\current_thread])+1)
  ** (\old(held[\current_thread]) > 0 ==> \result)
  ** (\old(held[\current_thread])==0 && \result ==> subject.inv());
  @*/
  boolean trylock(){
    int c, tmp, myid; boolean res;
    myid=currentThread();
    c=count.get();
    if (c==0) {
      res=count.cas(0,1)/*@ then { if (\result) {
        holder=\current_thread;
        held[\current_thread]=1;
      }} @*/;
      if (res) { owner.set(myid); }
      return res;
    } else {
      tmp=owner.get();
      if (tmp!=myid) { return false; }
      count.set(c+1)/*@ then {
        held[\current_thread]=held[\current_thread]+1;
      } @*/;
      return true;
    }
  }
// end(tryacquire)

/*@  
  requires common() ** lockset_part() ** held[\current_thread]>0;
  requires held[\current_thread]==1 ==> subject.inv();
  ensures  common() ** lockset_part() **
    held[\current_thread] == \old(held[\current_thread])-1;
  @*/
  void release(){
    int c;
    c=count.get()/*@
      with {
        csl_subject this;
      }
    @*/;
    c=c-1;
    if (c==0) {
      owner.set(-1);
    }
    count.set(c)/*@ then {
        held[\current_thread]=c;
        if (c==0) {
          holder=-1;
        }
    } @*/;
  }
//end(all)
  Lock(){
    //@ assume false;
  }
//begin(all)
}
//end(all)


