package vct.col.veymont;

import java.util.Objects;

public class WellBehavednessJava {

    public static boolean isTau(String label) {
        return label.equals("true @ Tau");
    }

    public static boolean checkForwardNonTau(AldebaranTau a) {
        return a.statesAllMatch(i ->

                // For all (i, label1, j1) and (i, tau^*, j2) ...
                a.transitionsAllMatch(i, (label1, j1) ->
                        a.tauClosureAllMatch(i, j2 -> {

                            // There exist (j1, tau^*, k1) and (j2, label2, k2) ...
                            if (isTau(label1) || a.tauClosureAnyMatch(j1, k1 ->
                                    a.transitionsAnyMatch(j2, (label2, k2) ->

                                            // Such that:
                                            Objects.equals(label1, label2) && Objects.equals(k1, k2)))) {
                                return true;
                            } else {
                                return false;
                            }
                        })));
    }

    public static boolean checkForwardTau(AldebaranTau a) {
        return a.statesAllMatch(i ->

                // For all (i, label1, j1) and (i, tau^*, j2) ...
                a.transitionsAllMatch(i, (label1, j1) ->
                        a.tauClosureAllMatch(i, j2 -> {

                            // There exist (j1, tau^*, k1) and (j2, tau^*, k2) ...
                            if (!isTau(label1) || a.tauClosureAnyMatch(j1, k1 ->
                                    a.tauClosureAnyMatch(j2, k2 ->

                                            // Such that:
                                            Objects.equals(k1, k2)))) {
                                return true;
                            } else {
                                return false;
                            }
                        })));
    }

    public static boolean checkBackward(AldebaranTau a) {
        return a.statesAllMatch(i ->

                // For all (i, tau*, j1) and (j1, label1, k1):
                a.tauClosureAllMatch(i, j1 ->
                        a.transitionsAllMatch(j1, (label1, k1) -> {

                            // There exist (i, label2, j2) and (j2, tau*, k2):
                            if (label1.contains("BarrierWait") || a.transitionsAnyMatch(i, (label2, j2) ->
                                    a.tauClosureAnyMatch(j2, k2 ->

                                            // Such that:
                                            Objects.equals(label1, label2) && Objects.equals(k1, k2)))) {
                                return true;
                            } else {
                                return false;
                            }
                        })));
    }

    public static boolean check(AldebaranTau a) {
        return checkForwardNonTau(a) && checkForwardTau(a) && checkBackward(a);
    }

}
