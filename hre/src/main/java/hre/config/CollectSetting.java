package hre.config;

import java.util.HashMap;

public class CollectSetting {
    private HashMap<String, Integer> settings = new HashMap<>();

    public AddOption getAddOption(String help) {
        return new AddOption(help);
    }

    public ExplicitOption getExplicitOption(String item, String help) {
        return new ExplicitOption(item, help);
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

    private class AddOption extends AbstractOption {
        AddOption(String help) {
            super(true, true, help);
        }

        @Override
        public void pass(String value) {
            settings.put(value, settings.getOrDefault(value, 0) + 1);
        }
    }

    private class ExplicitOption extends AbstractOption {
        private String item;

        ExplicitOption(String item, String help) {
            super(false, false, help);
            this.item = item;
        }

        @Override
        public void pass() {
            settings.put(item, settings.getOrDefault(item, 0) + 1);
        }
    }
}
