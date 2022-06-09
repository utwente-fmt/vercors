// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases BadLoop1
//:: tools silicon
//:: verdict Fail
class Counter {
  private int val;
  /*@ 
    requires Perm(val,1) ** n>=0;
    ensures Perm(val,1) ** val==\old(val)+n; 
  @*/
  void incr(int n)
  {
    int tmp=n;
    /*@ 
      loop_invariant [/expect perm]val[/end] + tmp==\old(val)+n && tmp>0;
      loop_invariant Perm(val,1); 
	@*/
    while(tmp>0)
    {
      val=val+1;
      tmp=tmp-1;
    }
  }
}

