// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases Counter
//:: tools silicon
//:: verdict Pass
/*
check with vct --silicon Counter.java
*/
class Counter {
  private int val;

  /*@
  ensures Perm(val, 1);
  ensures this.val == 0;
  @*/
  public Counter() {
      this.val = 0;
  }

  /*@ 
    requires Perm(val,1);
    ensures  Perm(val,1) ** val==\old(val)+1;
  @*/
  void incr(){
    val = val+1;
  }
  
  /*@ 
	requires Perm(val,1) ** n>=0;
    ensures Perm(val,1) ** val==\old(val)+n; 
  @*/
  void incr_by_n(int n)
  {
    int tmp=n;
    /*@ 
      loop_invariant Perm(val,1);
      loop_invariant val+tmp==\old(val)+n ** tmp>=0; 
    @*/
    while(tmp>0)
    {
      val=val+1;
      tmp=tmp-1;
    }
  }

  /*@ 
    requires Perm(c.val,1) ** n>=0;
    ensures Perm(c.val,1) ** c.val==\old(c.val)+n; 
  @*/
  static void incr_static(Counter c,int n)
  {
    int tmp=n;
    /*@ 
      loop_invariant Perm(c.val,1);
      loop_invariant c.val+tmp==\old(c.val)+n ** tmp>=0; 
    @*/
    while(tmp>0)
    {
      c.incr();
      tmp=tmp-1;
    }
  }

  static void main(){
    Counter c=new Counter();
    assert c.val==0;
  }
}

