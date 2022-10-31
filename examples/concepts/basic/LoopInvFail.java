//:: cases LoopInvBoogieFail
//:: tools silicon
//:: verdict Fail

public class LoopInv {
  /*@
    requires n > 0;
    ensures \result == n * n;
  @*/
  public static int f_bad(int n) {
    int res, i;
    res = 0;
    i = 0;
    //@ loop_invariant res == i * n;
    //@ loop_invariant [/expect notMaintained:false] i < n [/end];
    while (i < n) {
      res = res + n;
      i = i + 1;
    }
    return res;
  }
}
