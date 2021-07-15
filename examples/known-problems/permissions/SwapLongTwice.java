// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases SwapLong
//:: suite problem-fail
//:: tools silicon
//:: verdict Pass

// Issue with Long things in VerCors

public class SwapLong {

  long F;
  long G;
  /*@ 
    requires Perm(F,1) ** Perm(G,1);
    ensures Perm(F,1) ** Perm(G,1);
    ensures F == \old(G) && G == \old(F);
  @*/
  void n()
  {
    long tmp;
    tmp = F;
    F = G;
    G = tmp;
  }


  /*@ 
    requires Perm(F,1) ** Perm(G,1);
    ensures Perm(F,1) ** Perm(G,1);
    ensures F == \old(F) && G == \old(G);
  @*/
  void twice()
  {
    n();
    n();
  }


}

