//:: cases TC_PST_14_JavaRequiresConflictsEnsures
//:: tools silicon
//:: verdict Pass

// Java counterpart of TC-PST-11, with requires false isolating the postSat check.
public class TC_PST_14_JavaRequiresConflictsEnsures {
    //@ requires false;
    //@ ensures x < 0;
    public static void f(int x) { }
}
