// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases Separate

//:: cases SeparateUtil
//:: tools silicon
//:: verdict Fail

package separate;

public class Util {


    //@ ensures \result==x+1;
    public static int incr(int x) {
        return x + 2;
    }
}
