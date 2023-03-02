package hre.config;

import java.util.Collections;
import java.util.HashSet;

import static hre.lang.System.Abort;

public class ChoiceSetting {
    private String setting;
    private HashSet<String> allowedSettings;

    public ChoiceSetting(String[] settings, String devault) {
        this.setting = devault;
        this.allowedSettings = new HashSet<>();
        Collections.addAll(this.allowedSettings, settings);
    }

    private class SetOption extends AbstractOption {
        SetOption(String help) {
            super(true, true, help);
        }

        @Override
        public void pass(String value) {
            if(allowedSettings.contains(value)) {
                setting = value;
            } else {
                Abort("%s is not a valid value for this option. Choices are: %s", value, String.join(", ", allowedSettings));
            }
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
            setting = item;
        }
    }

    public SetOption getSetOption(String help) {
        return new SetOption(help);
    }

    public ExplicitOption getExplicitOption(String item, String help) {
        return new ExplicitOption(item, help);
    }

    public String get() {
        return setting;
    }

    public boolean is(String value) {
        return value.equals(setting);
    }
}
