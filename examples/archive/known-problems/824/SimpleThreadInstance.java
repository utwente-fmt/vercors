// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases SimpleThread
public class SimpleThreadInstance extends SimpleThread {

  /*@
    ensures preFork() ** Value(this.input) ** this.input==input;
  @*/
  public SimpleThreadInstance(int input){
    this.input=input;
    //@ fold preFork@SimpleThread();
    //@ fold preFork@SimpleThreadInstance();
  }

  //@ resource joinToken()=true;

  //@ resource preFork()=Value(input)**Perm(output,write);

  //@ resource postJoin()=Value(input)**PointsTo(output,write,input+1);
  

  int input;
  int output;
  
  public void run(){
    //@ unfold preFork@SimpleThreadInstance();
    output=input+1;
    //@ fold postJoin@SimpleThreadInstance();
  }
 
}

