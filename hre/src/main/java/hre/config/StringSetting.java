package hre.config;

public class StringSetting {

    private String value;

    private boolean opt_used = false;

    public StringSetting(String default_setting) {
        value = default_setting;
    }

    public boolean used() {
        return opt_used;
    }

    public Option getAssign(String help) {
        return new AssignOption(this, help);
    }

    public Option getAssign(String help, String value) {
        return new AssignOption(this, help, value);
    }

    public String get() {
        return value;
    }

    public void set(String value) {
        this.value = value;
    }

    private class AssignOption extends AbstractOption {
        StringSetting about;
        String default_value;

        public AssignOption(StringSetting about, String help, String value) {
            super(value == null, true, help);
            this.about = about;
            this.default_value = value;
        }

        public AssignOption(StringSetting about, String help) {
            super(value == null, true, help);
            this.about = about;
            this.default_value = null;
        }

        public void pass(String value) {
            opt_used = true;
            used = true;
            about.value = value;
        }

        public void pass() {
            opt_used = true;
            used = true;
            about.value = default_value;
        }
    }
}
