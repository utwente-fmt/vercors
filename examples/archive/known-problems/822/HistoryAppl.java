// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases HistoryApplication
//:: tools silicon
//:: options --check-history

class Application {

  /*@
    given process p;
    given frac q;
    requires q != none;
    requires Hist(h,q,p);
    requires HPerm(h.x,1);
    ensures  HPerm(h.x,1);
    ensures  Hist(h,q,p*h.incr());
  @*/
  void do_incr(History h){
    {
      //@ action h , q , p , h.incr();
      h.x=h.x+1;
    }
  }

  void main(){
    History h=new History();
    h.x=1;
    //@ create h;
    //@ split h, 1\3, empty, 2\3, empty;
    do_incr(h) /*@ with { p = empty; q = 1\3; } @*/;
    //@ merge h, 2\3, empty, 1\3, h.single(1);
    //@ assert AbstractState(h,x==1);
    //@ destroy h , h.single(1) ;
    //@ assert h.x==2;
  }

}