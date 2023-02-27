// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases TestScale
//:: tools silicon
//:: verdict Fail

class C {

  /*@
    resource P();

    requires ([1\2]P()) ** ([read]P());
    pure int good_order();
    
    requires ([read]P()) ** ([1\2]P());
    pure int bad_order();
  @*/

  /*@
    requires P();
    requires good_order()>0;
    requires bad_order()>0;
  @*/
  void main(){
  }
  
}

