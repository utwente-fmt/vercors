//:: cases TC_CB_10_JavaSelfContradicting
//:: tools silicon
//:: verdict Fail

// TC-CB-10: TC-CB-2 repeated in Java to verify COL transformation handles
// the self-contradicting condition correctly from the Java frontend.
public class TC_CB_10_JavaSelfContradicting {
    public static void f(int x) {
        if (x > 0 && x < 0) {
            x += 1; // dead: impossible condition
        }
    }
}
