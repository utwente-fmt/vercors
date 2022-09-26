package hre.config;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

public class LinearCollectSetting {
    private List<String> settings = new ArrayList<>();

    private class AddOption extends AbstractOption {
        AddOption(String help) {
            super(true, true, help);
        }

        @Override
        public void pass(String value) {
            settings.add(value);
        }
    }

    public AddOption getAddOption(String help) {
        return new AddOption(help);
    }

    public List<String> get() {
        return new ArrayList<>(settings);
    }

    public boolean has(String s) {
        return settings.contains(s);
    }

    public int count(String s) {
        return (int) settings.stream().filter(elem -> elem == s).count();
    }
}
