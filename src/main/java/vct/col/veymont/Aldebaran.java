package vct.col.veymont;

import java.util.*;
import java.util.function.BiConsumer;
import java.util.function.BiPredicate;
import java.util.function.Consumer;
import java.util.function.Predicate;

public class Aldebaran {

    private final int s0;
    private final int states;
    private final Map<Integer, Map<String, List<Integer>>> transitions;

    public Aldebaran(int s0, int states, Map<Integer, Map<String, List<Integer>>> transitions) {
        this.s0 = s0;
        this.states = states;
        this.transitions = transitions;
    }

    public Aldebaran(Scanner in) {
        if (!in.hasNextLine()) throw new IllegalArgumentException();

        String l;

        /*
         * Parse header
         */

        l = in.nextLine();
        if (!l.startsWith("des (") || !l.endsWith(")")) throw new IllegalArgumentException();

        l = l.substring(5, l.length() - 1);
        var tokens = l.split(",");
        if (tokens.length != 3) throw new IllegalArgumentException();

        try {
            s0 = Integer.parseInt(tokens[0]);
            states = Integer.parseInt(tokens[2]);
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException();
        }

        /*
         * Parse transitions
         */

        transitions = new LinkedHashMap<>(states, 1);

        while (in.hasNextLine()) {
            l = in.nextLine();
            if (!l.startsWith("(") || !l.endsWith(")")) throw new IllegalArgumentException();

            l = l.substring(1, l.length() - 1);
            if (l.indexOf(",") == l.lastIndexOf(",")) throw new IllegalArgumentException();
            int i, j;
            try {
                i = Integer.parseInt(l.substring(0, l.indexOf(",")));
                j = Integer.parseInt(l.substring(l.lastIndexOf(",") + 1));
            } catch (NumberFormatException e) {
                throw new IllegalArgumentException();
            }

            var label = l.substring(l.indexOf(",") + 1, l.lastIndexOf(","));
            if (!label.startsWith("\"") || !label.endsWith("\"")) throw new IllegalArgumentException();
            label = label.substring(1, label.length() - 1);

            if (!transitions.containsKey(i)) transitions.put(i, new LinkedHashMap<>());
            var iTransitions = transitions.get(i);
            if (!iTransitions.containsKey(label)) iTransitions.put(label, new ArrayList<>());
            iTransitions.get(label).add(j);
        }

        for (int i = 0; i < states; i++) {
            if (!transitions.containsKey(i)) transitions.put(i, new LinkedHashMap<>());
        }

    }

    public boolean statesAllMatch(Predicate<Integer> p) {
        return transitions.keySet().stream().allMatch(p);
    }

    public void statesForEach(Consumer<Integer> c) {
        transitions.keySet().forEach(c);
    }

    public boolean transitionsAllMatch(Integer i, BiPredicate<String, Integer> p) {
        if (!transitions.containsKey(i)) throw new IllegalArgumentException();
        return transitions.get(i).entrySet().stream().allMatch(e ->
                e.getValue().stream().allMatch(j -> p.test(e.getKey(), j)));
    }

    public boolean transitionsAnyMatch(Integer i, BiPredicate<String, Integer> p) {
        if (!transitions.containsKey(i)) throw new IllegalArgumentException();
        return transitions.get(i).entrySet().stream().anyMatch(e ->
                e.getValue().stream().anyMatch(j -> p.test(e.getKey(), j)));
    }

    public void transitionsForEach(Integer i, BiConsumer<String, Integer> c) {
        if (!transitions.containsKey(i)) throw new IllegalArgumentException("" + i);
        transitions.get(i).entrySet().forEach(e ->
                e.getValue().forEach(j -> c.accept(e.getKey(), j)));
    }

    @Override
    public String toString() {
        StringBuilder s = new StringBuilder();
        var nTransitions = 0;
        for (int i : transitions.keySet()) {
            for (Map.Entry<String, List<Integer>> e : transitions.get(i).entrySet()) {
                var label = e.getKey();
                var js = e.getValue();
                nTransitions += js.size();
                for (int j : js) {
                    s.append("\n(").append(i).append(",\"").append(label).append("\",").append(j).append(")");
                }
            }
        }

        return "des (" + s0 + "," + nTransitions + "," + states + ")" + s;
    }
}
