// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: case NoSendAfterRead
//:: suite puptol
//:: tool silicon
//:: option --check-history
/*@
model Future {
  boolean flag;
  
  accessible flag; //skip(all)
  requires flag;
  action p_send();
  
  accessible flag; //skip(all)
  requires !flag;  //skip(all)
  action receive();

  modifies flag;  //skip(all)
  ensures !flag;
  action clear();
  
  requires true; //skip(all)
  ensures true;  //skip(all)
  process nsar()=p_send()*nsar() + clear()*rs();
  
  process rs()=clear()*rs() + receive()*rs();
}
@*/

class Device {
  Future F;

/*@
  ensures Value(F) ** HPerm(F.flag,1) ** F.flag;
  ensures F.state(1,F.nsar());
@*/
  public Device() {
    /*@ghost {
      F = new Future();
      F.flag = true;
      F.create(F.nsar());
    }@*/
  }

/*@
  given frac p; //skip(all)
  given process P; //skip(all)
  requires p!=none ** Value(F); //skip(all)
  requires HPerm(F.flag,p) ** F.flag ** F.state(p,F.p_send()*P);
  ensures  p!=none ** Value(F); //skip(all)
  ensures HPerm(F.flag,p) ** F.flag ** F.state(p,P);
@*/
  void send();

/*@ 
  given frac p; //skip(all)
  given process P; //skip(all)
  requires p!=none ** Value(F) ** HPerm(F.flag,p)//skip(all)
    ** !F.flag; //skip(all)
  requires F.state(p,F.receive()*P);
  ensures  p!=none ** Value(F) ** HPerm(F.flag,p) ** !F.flag; //skip(all)
  ensures F.state(p,P);
@*/
  void receive();
}

class Lock {
  //@ ghost Device d;
  
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
     requires d.F.state(1\2,d.F.nsar());
  requires Value(l) ** Value(l.d) ** l.d==d; //skip(run)
@*/ public void run(){
/*@ loop_invariant Value(d) ** Value(d.F) ** Value(l)//skip(run)
    ** Value(l.d) ** l.d==d; //skip(run)
    loop_invariant d.F.state(1\2,d.F.nsar());
@*/ while(true){
      l.lock();
      if (l.flag){
        //@ ghost d.F.choose(1\2,d.F.nsar(),d.F.p_send()*d.F.nsar()); //skip(run)
        d.send /*@ given { p = 1\2, P = d.F.nsar() } @*/();
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
    requires d.F.state(1\2,d.F.rs());
@*/ public void run(){
/*@ loop_invariant Value(d) ** Value(d.F) ** Value(l) ** //skip(run)
      Value(l.d) ** l.d==d; // skip(run)  
    loop_invariant d.F.state(1\2,d.F.rs()); @*/
    while(true){
      l.lock();
      //@ ghost d.F.choose(1\2,d.F.rs(),d.F.clear()*d.F.rs()); //skip(run)
      { //@ action(d.F,1\2,d.F.rs(),d.F.clear());
        l.flag=false;
        //@ ghost d.F.flag=false;
      }
      //@ ghost d.F.choose(1\2,d.F.rs(),d.F.receive()*d.F.rs()); //skip(run)
      d.receive /*@ given { p = 1\2, P = d.F.rs() } @*/();
      l.unlock();
    }
  }
}

