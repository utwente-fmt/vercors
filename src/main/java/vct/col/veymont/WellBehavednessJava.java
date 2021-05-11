package vct.col.veymont;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.Objects;
import java.util.Scanner;

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
                                System.out.println("(" + i + ",\"" + label1 + "\"," + j1 + ") and (" + i + ",tau^*," + j2 + ")");
                                System.out.println();
                                System.out.println(a);
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
                                System.out.println("(" + i + ",tau^*," + j1 + ") and (" + i + ",tau^*," + j2 + ")");
                                System.out.println();
                                System.out.println(a);
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
                                System.out.println("(" + i + ",tau^*," + j1 + ") and (" + j1 + ",\"" + label1 + "\"," + k1 + ")");
                                System.out.println();
                                System.out.println(a);

                                return false;
                            }
                        })));
    }

    public static boolean check(AldebaranTau a) {
        return checkForwardNonTau(a) && checkForwardTau(a) && checkBackward(a);
    }

    public static void main(String[] args) throws FileNotFoundException {
        var examples = new String[]{
                "../program-ltss/ifelseaLocalLTS.aut",
                "../program-ltss/ifelsebLocalLTS.aut",
                "../program-ltss/ifelsecLocalLTS.aut",

                "../program-ltss/whileaLocalLTS.aut",
                "../program-ltss/whilebLocalLTS.aut",
                "../program-ltss/whilecLocalLTS.aut",

                "../program-ltss/parallel_whileaLocalLTS.aut",
                "../program-ltss/parallel_whilebLocalLTS.aut",
                "../program-ltss/parallel_whilecLocalLTS.aut",

                "../program-ltss/methodcallaLocalLTS.aut",
                "../program-ltss/methodcallbLocalLTS.aut",
                "../program-ltss/methodcallcLocalLTS.aut",

                "../program-ltss/TicTacToeP1LocalLTS.aut",
                "../program-ltss/TicTacToeP2LocalLTS.aut",

                "../program-ltss/paperscissorsrockaLocalLTS.aut",
                "../program-ltss/paperscissorsrockbLocalLTS.aut",
                "../program-ltss/paperscissorsrockcLocalLTS.aut",
        };

        for (String s : examples) {
            var f = new File(s);
            var b = check(new AldebaranTau(new Scanner(f), WellBehavednessJava::isTau));
            System.out.println(b + " (" + s + ")");
        }
    }
}
