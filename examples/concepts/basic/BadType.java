// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases BadTypeJava
//:: tools silicon
//:: verdict Error
class BadType {
  
  /*@
    given frac p;
    requires Perm(x,1) ** Perm(x,p) ** p>0;
  @*/
  void m(int x){
  }

}
