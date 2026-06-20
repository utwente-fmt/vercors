//:: cases TC_CB_11_CSelfContradicting
//:: tools silicon
//:: verdict Fail

// TC-CB-11: TC-CB-2 repeated in C to verify COL transformation handles
// the self-contradicting condition correctly from the C frontend.
void f(int x) {
    if (x > 0 && x < 0) {
        x = 1;
    }
}
