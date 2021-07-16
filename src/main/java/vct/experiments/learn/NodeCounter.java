package vct.experiments.learn;

import java.util.Map;
import java.util.TreeMap;

public class NodeCounter {

    private Map<String, Integer> counts;

    public NodeCounter() {
        this.counts = new TreeMap<String, Integer>();
    }

    public void count(String... tags) {
        String label = String.join(":", tags);
        counts.put(label, counts.getOrDefault(label, 0) + 1);
    }

    public int sum() {
        return counts.values().stream().mapToInt(Integer::intValue).sum();
    }

    public Map<String, Integer> getCounts() {
        return counts;
    }

}