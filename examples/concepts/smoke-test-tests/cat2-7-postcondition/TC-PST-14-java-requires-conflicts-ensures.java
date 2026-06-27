//:: cases TC_PST_14_JavaRequiresConflictsEnsures
//:: tools silicon
//:: verdict Pass

// requires false makes normal verification vacuous, isolating the postSat check:
// ensures x < 0 is satisfiable on its own (e.g. x = -1), so postSat must NOT fire.
// PVL counterpart: TC-PST-11.
public class TC_PST_14_JavaRequiresConflictsEnsures {
    //@ requires false;
    //@ ensures x < 0;
    public static void f(int x) { }
}
