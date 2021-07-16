package hre.config;

public class IntegerSetting {

    private int value;

    public IntegerSetting(int default_setting) {
        value = default_setting;
    }

    public Option getAssign(String help) {
        return new AssignOption(this, help, true);
    }

    public Option getOptionalAssign(String help) {
        return new AssignOption(this, help, false);
    }

    public int get() {
        return value;
    }

    public void set(int value) {
        this.value = value;
    }

    private class AssignOption extends AbstractOption {
        IntegerSetting about;

        public AssignOption(IntegerSetting about, String help, boolean required) {
            super(required, true, help);
            this.about = about;
        }

        public void pass(String value) {
            used = true;
            about.value = Integer.parseInt(value);
        }
    }
}
