//:: cases TC_PST_14_JavaRequiresConflictsEnsures
//:: tools silicon
//:: verdict Pass

// TC-PST-14 (Java): In VerCors, 'x' in an ensures clause refers to the original
// parameter value at the call site (frozen at entry), not the locally-modified copy.
// So requires x == 5; ensures x < 0 would always fail normal verification (5 < 0).
// requires false makes normal verification vacuous, isolating the postSat check:
// ensures x < 0 is satisfiable on its own (e.g. x = -1), so postSat must NOT fire.
// TC-PST-11 (PVL) counterpart.
public class TC_PST_14_JavaRequiresConflictsEnsures {
    //@ requires false;
    //@ ensures x < 0;
    public static void f(int x) { }
}
