// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases FullThread
public class Instance extends Thread {

  /*@
    ensures pre:preFork(write) ** Value(this.input) ** this.input==input;
  @*/
  public Instance(int input){
    //@ witness sup:this.preFork@Thread(write);
    this.input=input;
    //@ fold pre:this.preFork@Instance(write);
  }

  //@ resource preFork(frac p)=Value(input)**Perm(output,p);

  //@ resource postJoin(frac p)=Value(input)**PointsTo(output,p,input+1);
  

  int input;
  int output;
  
  public void run(){
    //@ witness sup:this.postJoin@Thread(write);
    //@ unfold pre:preFork(write);
    output=input+1;
    //@ fold post:this.postJoin@Instance(write);
  }
 
}

