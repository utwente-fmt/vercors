package hre.ast;

public class InlineOrigin extends Origin {


    public final Origin location;
    public final Origin main;

    public InlineOrigin(Origin location, Origin main) {
        this.location = location;
        this.main = main;
    }

    @Override
    public void report(String level, Iterable<String> message) {
        location.report(level, "in definition of");
        main.report(level, message);
    }

    @Override
    public void report(String level, String... message) {
        location.report(level, "in definition of");
        main.report(level, message);
    }

}
