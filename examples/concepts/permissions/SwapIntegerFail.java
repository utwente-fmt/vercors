//:: cases SwapIntegerFail
//:: tools silicon
//:: verdict Fail

public class SwapInteger {

  int F;
  int G;
  /*@
    requires Perm(F,1) ** Perm(G,1);
    ensures Perm(F,1) ** Perm(G,1);
    ensures F == \old(G) && G == \old(F);
  @*/
  void n()
  {
    int tmp;
    tmp = F;
    F = G;
    G = tmp;
  }

  /*@
    requires Perm(F,1) ** Perm(G,1);
    ensures Perm(F,1) ** Perm(G,1);
    ensures F == \old(F) && G == \old(G);
  @*/
  void wrong()
  {
    n();
  }

}

