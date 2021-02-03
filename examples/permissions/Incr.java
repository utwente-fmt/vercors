// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: case IncrSilicon
//:: tools silicon
//:: verdict Pass Incr.incr_n_ok Incr.incr_ok
//:: verdict Fail Incr.incr_n_badinv

class Incr {
  int val;
  
  /*@ 
    requires Perm(val,1);
    ensures Perm(val,1) ** val == \old(val)+1;
  @*/
  void incr_ok()
  {
    val = val+1;
  }

  /*@ 
    requires Perm(val,1) **  n>=0;
    ensures Perm(val,1) ** val == \old(val)+n; 
  @*/
  void incr_n_ok(int n)
  {
    int tmp;
    tmp=n;
    /*@ 
      loop_invariant Perm(val,1);
      loop_invariant val+tmp == \old(val)+n && tmp>=0; 
	@*/
    while(tmp>0)
    {
      val=val+1;
      tmp=tmp-1;
    }
  }

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

