//:: cases TC_PST_13_JavaEnsuresUnsat
//:: tools silicon
//:: verdict Fail

// Same as TC-PST-2 but via Java.
public class TC_PST_13_JavaEnsuresUnsat {
    //@ ensures \result > 0 && \result < 0;
    public static int f(int x) {
        return 1;
    }
}
