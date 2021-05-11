package vct.col.veymont;

import java.util.*;
import java.util.function.Predicate;
import java.util.function.Supplier;

public class AldebaranTau extends Aldebaran {

    private final Map<Integer, Set<Integer>> tauClosure = new HashMap<>();

    public AldebaranTau(int s0, int states, Map<Integer, Map<String, List<Integer>>> transitions, Predicate<String> isTau) {
        super(s0, states, transitions);
        init(isTau);
    }

    public AldebaranTau(Scanner in, Predicate<String> isTau) {
        super(in);
        init(isTau);
    }

    public boolean tauClosureAllMatch(Integer i, Predicate<Integer> p) {
        if (!tauClosure.containsKey(i)) throw new IllegalArgumentException();
        return tauClosure.get(i).stream().allMatch(p);
    }

    public boolean tauClosureAnyMatch(Integer i, Predicate<Integer> p) {
        if (!tauClosure.containsKey(i)) throw new IllegalArgumentException();
        return tauClosure.get(i).stream().anyMatch(p);
    }

    private void init(Predicate<String> isTau) {
        statesForEach(i -> {
            var todo = new Stack<Supplier<Integer>>();
            todo.push(() -> i);
            while (!todo.isEmpty()) {
                var j = todo.pop().get();
                if (!tauClosure.containsKey(j)) {
                    var ks = new HashSet<Integer>();
                    transitionsForEach(j, (label, k) -> {
                        if (isTau.test(label)) ks.add(k);
                    });
                    todo.push(() -> {
                        var ls = new HashSet<Integer>();
                        ls.add(j);
                        ks.forEach(k -> ls.addAll(tauClosure.get(k)));
                        tauClosure.put(j, ls);
                        return j;
                    });
                    ks.forEach(k -> todo.push(() -> k));
                }
            }
        });
//        System.out.println(tauClosure);
    }
}
