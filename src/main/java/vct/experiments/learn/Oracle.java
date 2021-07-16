package vct.experiments.learn;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.reflect.TypeToken;
import com.google.gson.stream.JsonReader;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.lang.reflect.Type;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.Map.Entry;

import static hre.lang.System.DebugException;
import static hre.lang.System.Warning;

public class Oracle {

    private static final double learn_factor = 0.25;

    public static void tell(String name, SpecialCountVisitor scv, long time) {
        double unit_time = ((double) time) / scv.getCounter().sum();
        tell(name, scv.getCounter(), unit_time);
        tell(name, scv.getSpecialCounter(), unit_time);
    }

    public static void tell(String name, NodeCounter counter, double unit_time) {
        Map<String, Double> times = getMap(name);
        Map<String, Double> diffs = getMap(name + "_diff");
        for (Entry<String, Integer> entry : counter.getCounts().entrySet()) {
            double val = unit_time;
            double old = times.getOrDefault(entry.getKey(), val);
            double diff = Math.abs(old - val);
            times.put(entry.getKey(), newValue(old, val));
            diffs.put(entry.getKey(), newValue(diffs.getOrDefault(entry.getKey(), diff), diff));
        }
        saveMap(name, sortByValue(times));
        saveMap(name + "_diff", sortByValue(diffs));
    }

    private static double newValue(double oldValue, double newValue) {
        return (learn_factor * newValue) + ((1 - learn_factor) * oldValue);
    }

    private static Map<String, Double> getMap(String name) {
        Map<String, Double> result = null;
        Path p = new File(".learn").toPath().resolve(name + ".json");
        File f = p.toFile();
        if (f.exists()) {
            Gson gson = new GsonBuilder().create();
            try {
                JsonReader r = gson.newJsonReader(new FileReader(f));
                Type t = new TypeToken<Map<String, Double>>() {
                }.getType();
                result = gson.fromJson(r, t);
            } catch (FileNotFoundException e) {
                DebugException(e);
            }
        }
        if (result == null) {
            result = new TreeMap<String, Double>();
        }
        return result;
    }

    private static void saveMap(String name, Map<String, Double> map) {
        Gson gson = new GsonBuilder().setPrettyPrinting().create();
        String json = gson.toJson(map);
        List<String> lines = new ArrayList<String>();
        lines.add(json);
        try {
            Path p = new File(".learn").toPath().resolve(name + ".json");
            File f = p.toFile();
            if (!f.exists()) {
                if (!f.getParentFile().mkdirs() || !f.createNewFile()) {
                    throw new IOException();
                }
            }
            Files.write(p, lines, Charset.forName("UTF-8"));
        } catch (IOException e) {
            DebugException(e);
            Warning("Could not save learned times to .learn/%s.json", name);
        }
    }

    public static <K, V extends Comparable<? super V>> Map<K, V> sortByValue(Map<K, V> map) {
        List<Entry<K, V>> list = new ArrayList<>(map.entrySet());
        list.sort(Entry.comparingByValue());
        Collections.reverse(list);

        Map<K, V> result = new LinkedHashMap<>();
        for (Entry<K, V> entry : list) {
            result.put(entry.getKey(), entry.getValue());
        }

        return result;
    }

}
