package hre.config;

import java.util.HashMap;
import java.util.HashSet;

public class CollectSetting {
    private HashMap<String, Integer> settings = new HashMap<>();

    private class AddOption extends AbstractOption {
        AddOption(String help) {
            super(true, true, help);
        }

        @Override
        public void pass(String value) {
            settings.put(value, settings.getOrDefault(value, 0) + 1);
        }
    }

    public AddOption getAddOption(String help) {
        return new AddOption(help);
    }

    public HashMap<String, Integer> get() {
        return settings;
    }

    public boolean has(String s) {
        return settings.containsKey(s);
    }

    public int count(String s) {
        return settings.getOrDefault(s, 0);
    }
}
