//:: case IncrSiliconFail
//:: tools silicon
//:: verdict Fail

class Incr {
  int val;
  
  /*@
    requires Perm(val,1) ** n>=0;
    ensures Perm(val,1) ** val==\old(val)+n; 
  @*/
  void incr_n_badinv(int n)
  {
    int tmp;
    tmp=n;
    /*@ 
      loop_invariant val+tmp == \old(val)+n && tmp>0;
      loop_invariant Perm(val,1); 
	@*/
    while(tmp>0)
    {
      val=val+1;
      tmp=tmp-1;
    }
  }

}

