// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: case NoSendAfterRead
//:: suite puptol
//:: tool silicon
//:: option --check-history

public class Future {/*@
  boolean flag;
  
  accessible flag; //skip(all)
  requires flag;
  process send();
  
  accessible flag; //skip(all)
  requires !flag;  //skip(all)
  process receive();

  modifies flag;  //skip(all)
  ensures !flag;
  process clear();
  
  requires true; //skip(all)
  ensures true;  //skip(all)
  process nsar()=send()*nsar()+clear()*rs();
  
  process rs()=clear()*rs()+receive()*rs();

@*/}

class Device {
  Future F;

/*@
  ensures Value(F) ** HPerm(F.flag,1) ** F.flag;
  ensures Future(F,1,F.nsar());
@*/
  public Device() {
    /*@
      F = new Future();
      F.flag = true;
      create F, F.nsar();
    @*/
  }

/*@
  given frac p; //skip(all)
  given process P; //skip(all)
  requires p!=none ** Value(F); //skip(all)
  requires HPerm(F.flag,p) ** F.flag ** Future(F,p,F.send()*P);
  ensures  p!=none ** Value(F); //skip(all)
  ensures HPerm(F.flag,p) ** F.flag ** Future(F,p,P);
@*/
  void send();

/*@ 
  given frac p; //skip(all)
  given process P; //skip(all)
  requires p!=none ** Value(F) ** HPerm(F.flag,p)//skip(all)
    ** !F.flag; //skip(all)
  requires Future(F,p,F.receive()*P); 
  ensures  p!=none ** Value(F) ** HPerm(F.flag,p) ** !F.flag; //skip(all)
  ensures Future(F,p,P);
@*/
  void receive();
}

class Lock {
  //@ Device d;
  
  boolean flag;
  
  /*@ inline resource inv()=
        Value(d)**Perm(flag,write)**Value(d.F)**
        HPerm(d.F.flag,write)**d.F.flag==flag; @*/
  
  //@ ensures inv();
  void lock();
  
  //@ requires inv();
  void unlock();

}

class Sender {
  Device d;

  Lock l;

/*@  
     requires Value(d) ** Value(d.F); //skip(run)
     requires Future(d.F,1\2,d.F.nsar()); 
  requires Value(l) ** Value(l.d) ** l.d==d; //skip(run)
@*/ public void run(){
/*@ loop_invariant Value(d) ** Value(d.F) ** Value(l)//skip(run)
    ** Value(l.d) ** l.d==d; //skip(run)
    loop_invariant Future(d.F,1\2,d.F.nsar());
@*/ while(true){
      l.lock();
      if (l.flag){
        //@ choose d.F,1\2,d.F.nsar(),d.F.send()*d.F.nsar(); //skip(run)
        d.send()
            /*@ with { p=1\2; P=d.F.nsar();} @*/; //skip(run)
      }
      l.unlock();
    }
  }
}

class Reader {
  Device d;

  Lock l;
/*@ requires Value(d) ** Value(l) ** Value(l.d) ** //skip(run)
     l.d==d ** Value(d.F); //skip(run)
    requires Future(d.F,1\2,d.F.rs());
@*/ public void run(){
/*@ loop_invariant Value(d) ** Value(d.F) ** Value(l) ** //skip(run)
      Value(l.d) ** l.d==d; // skip(run)  
    loop_invariant Future(d.F,1\2,d.F.rs()); @*/
    while(true){
      l.lock();
      //@ choose d.F,1\2,d.F.rs(),d.F.clear()*d.F.rs(); //skip(run)
      { //@ action d.F,1\2,d.F.rs(),d.F.clear();
        l.flag=false;
        //@ d.F.flag=false;
      }
      //@ choose d.F,1\2,d.F.rs(),d.F.receive()*d.F.rs(); //skip(run)
      d.receive()
          /*@ with { p=1\2; P=d.F.rs();} @*/; //skip(run)
      l.unlock();
    }
  }
}

