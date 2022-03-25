// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases SwapLongWrong
//:: suite problem-fail
//:: tools silicon
//:: verdict Fail

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

  /*[/expect postFailed]*/
  /*@ 
    requires Perm(F,1) ** Perm(G,1);
    ensures Perm(F,1) ** Perm(G,1);
    ensures F == \old(F) && G == \old(G);
  @*/
  void wrong()
  {
    n();
  }
  /*[/end]*/
}

