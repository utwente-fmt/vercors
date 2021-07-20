//:: cases LoopInvBoogiePass
//:: tools silicon
//:: verdict Pass

public class LoopInv {
  /*@ 
    requires n > 0;
    ensures \result == n * n; 
  @*/
  public static int f_ok(int n) {
    int res, i;
    res = 0;
    i = 0;
    //@ loop_invariant res == i * n;
    //@ loop_invariant i <= n;
    while (i < n) {
      res = res + n;
      //i++;
      i = i + 1;
    }
    return res;
  }
}
