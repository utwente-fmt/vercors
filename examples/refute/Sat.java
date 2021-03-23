// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases RefuteSat
//:: tools silicon
//:: verdict Pass

/*
  Everything is corrrect.
*/
public class Sat {

  // satisfiable is good.
  /*@
    requires true;
  @*/
  public void good(){
  }

  // intentionally requiring false is acceptable too.
  /*@
    requires false;
  @*/
  public void good2(){
  }

}

