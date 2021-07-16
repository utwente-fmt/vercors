// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases Java6LockImplementation
//:: suite problem-fail
//:: tools silicon
//:: verdict Pass

/*
    vct --silver=silicon Java6Lock.java
 */
//begin(all)
final class AtomicInteger {

    int dummy;

    int val;

    int get() {
        return val;
    }

    void set(int v) {
        val = v;
    }

    boolean cas(int e, int v) {
        if (val == e) {
            val = v;
            return true;
        } else {
            return false;
        }
    }

}

final class Subject {

  /*@
    resource inv();
  @*/

}


final class Thread {

    int T;
    int L;
    Lock locks[];

    Thread() {
        //@ assume false;
    }

  /*@
    inline resource MyMyAPerm(loc<AtomicInteger> ref,frac p)=
      Value(ref)**Perm(ref.val,p);
    inline resource MyMyAPointsTo(loc<AtomicInteger> ref,frac p,int v)=
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
     Value({:locks[l]:}) ** Value(locks[l].subject) **
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
    boolean trylock(int lock_id) {
        boolean res;
        //@ unfold lockset(S);
        res = locks[lock_id].trylock();
    /*@ ghost
      if (res) {
        fold lockset(S+bag<int>{lock_id});
      } else {
        fold lockset(S);
      }
    @*/
        return res;
    }
}


final class Lock {

  /*@
    ghost int T; // Maximum number of threads.

    ghost int held[];
  
    ghost int holder;
    
    ghost Subject subject;
  @*/

    AtomicInteger count;

    AtomicInteger owner;

  /*@
    inline resource common()=
      Value(count) ** Value(owner) **
      Value(T) ** 0 <= \current_thread < T **
      Value(held) ** held != null **
      Value(subject);
      
    inline resource MyAPerm(loc<AtomicInteger> ref,frac p)=
      Value(ref)**Perm(ref.val,p);
    inline resource MyAPointsTo(loc<AtomicInteger> ref,frac p,int v)=
      Value(ref)**PointsTo(ref.val,p,v);
  @*/
  
/*@
// begin(context_everywhere)
resource csl_invariant()= Value(T) ** T > 0 **
   Value(held) ** held != null ** Value(subject) **
   MyAPerm(count,1\2) ** MyAPerm(owner,1\2) **
   (count.val == 0 ==> subject.inv() **
           MyAPerm(count,1\2) ** MyAPerm(owner,1\2)) **
   Perm(holder,1) ** -1 <= holder < T **
   (holder == -1) == (count.val == 0) **
   (\forall* int i; 0 <= i < T ;
     Perm({:held[i]:},1\2) ** (i!=holder ==> held[i]==0)
     ** held[i] >= 0 ** (held[i]==0 ==> owner.val!=i)
   );
// end(context_everywhere)
 @*/

  /*@
    inline thread_local
// begin(context_everywhere)
    resource lockset_part()= held!=null **
       Perm(held[\current_thread],1\2) **
       (held[\current_thread] > 0 ==>
          MyAPointsTo(count,1\2,held[\current_thread]) **
          MyAPointsTo(owner,1\2,\current_thread));
// end(context_everywhere)
  @*/


    //end(all)
    Lock() {
        //@ assume false;
    }

    /*@
      ensures \result==\current_thread;
    @*/
    public /*@ thread_local pure @*/ static int currentThread();
// end(tryacquire)

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
    boolean trylock() {
        int c, tmp, myid;
        boolean res;
        myid = currentThread();
        c = count.get();
        if (c == 0) {
            res = count.cas(0, 1)/*@ then { if (\result) {
        holder=\current_thread;
        held[\current_thread]=1;
      }} @*/;
            if (res) {
                owner.set(myid);
            }
            return res;
        } else {
            tmp = owner.get();
            if (tmp != myid) {
                return false;
            }
            count.set(c + 1)/*@ then {
        held[\current_thread]=held[\current_thread]+1;
      } @*/;
            return true;
        }
    }

    /*@
      requires common() ** lockset_part() ** held[\current_thread]>0;
      requires held[\current_thread]==1 ==> subject.inv();
      ensures  common() ** lockset_part() **
        held[\current_thread] == \old(held[\current_thread])-1;
      @*/
    void release() {
        int c;
        c = count.get()/*@
      with {
        csl_subject this;
      }
    @*/;
        c = c - 1;
        if (c == 0) {
            owner.set(-1);
        }
        count.set(c)/*@ then {
        held[\current_thread]=c;
        if (c==0) {
          holder=-1;
        }
    } @*/;
    }
//begin(all)
}
//end(all)


