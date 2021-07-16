package hre.config;

import static hre.lang.System.Fail;

public class BooleanSetting {

    private boolean value;

    public BooleanSetting(boolean default_setting) {
        value = default_setting;
    }

    public Option getEnable(String help) {
        return new EnableOption(help);
    }

    public Option getDisable(String help) {
        return new DisableOption(help);
    }

    public Option getAssign(String help) {
        return new AssignOption(help);
    }

    public boolean get() {
        return value;
    }

    public void set(boolean value) {
        this.value = value;
    }

    private class EnableOption extends AbstractOption {
        public EnableOption(String help) {
            super(false, false, help);
        }

        public void pass() {
            used = true;
            value = true;
        }
    }

    private class DisableOption extends AbstractOption {
        public DisableOption(String help) {
            super(false, false, help);
        }

        public void pass() {
            used = true;
            value = false;
        }
    }

    private class AssignOption extends AbstractOption {
        public AssignOption(String help) {
            super(true, true, help);
        }

        public void pass(String value) {
            used = true;
            if (value.equalsIgnoreCase("true") || value.equalsIgnoreCase("on")) {
                set(true);
            } else if (value.equalsIgnoreCase("false") || value.equalsIgnoreCase("off")) {
                set(false);
            }
            Fail("cannot parse %s", value);
        }
    }
}
