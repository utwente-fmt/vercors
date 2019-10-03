// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases WandDemoSilver
//:: tools silicon
/**
  The command line to verify with the VerCors Tool is:
  
  vct --silver=silicon WandDemoSilver.java
  
  The expected result is Pass.
*/
final class WandDemo {
  int x;

  /*@
    resource readonly()=Perm(x,1f/4);
    resource writeonly()=Perm(x,write);
  @*/
  
  /*@
    requires readonly();
    public pure int get()=\unfolding readonly() \in x;
  */
   
  //@ ensures writeonly();
  WandDemo(){
    //@ fold writeonly();
  }

  /*@
    requires writeonly();
    ensures  readonly();
    ensures  readonly()-*writeonly();
  @*/
  void set(int v){
    //@ unfold writeonly();
    x=v;
    //@ fold readonly();
    /*@
      create {
        unfold readonly();
        use    Perm(this.x,3f/4);
        fold   writeonly();
        qed    readonly()-*writeonly();
      }
    @*/
  }


  void demo(){
    WandDemo d=new WandDemo();
    int i=1;
    //@ loop_invariant d.writeonly();
    while(true)
    {
        d.set(i);
        i=d.get()+d.get();
        //@ apply d.readonly()-*d.writeonly();
    }
  }

}

