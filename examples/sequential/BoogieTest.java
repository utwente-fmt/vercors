//:: cases BoogieTestPass
//:: tools silicon
//:: verdict Pass

public class BoogieTest {

    /*@
      requires true;
      ensures \result >= 0 && (\result == x || \result == -x);
    @*/
    public static int abs(int x) {
        if (x > 0) return x;
        return -x;
    }

    /*@
      requires true;
      ensures \result == x+1;
    @*/
    public static int good_incr_1(int x) {
        int res = x + 1;
        return res;
    }

    /*@
      requires true;
      ensures \result == x+1;
    @*/
    public static int good_incr_2(int x) {
        int res = good_incr_1(x);
        return res;
    }

}

