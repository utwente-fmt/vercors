// -*- tab-width:2 ; indent-tabs-mode:nil -*-
package hre.ast;

import static hre.lang.System.Output;

/**
 * Origin that consists of a single text message.
 *
 * @author sccblom
 */
public class MessageOrigin extends Origin {

    private String message;

    public MessageOrigin(String format, Object... args) {
        this.message = String.format(format, args);
    }

    public MessageOrigin(String message) {
        this.message = message;
    }

    public String toString() {
        return message;
    }

    @Override
    public void report(String level, Iterable<String> message) {
        if (level.equals("result")) {
            return;
        }
        Output("=========================================");
        Output("%s in %s:", level, this.message);
        for (String line : message) {
            Output("  %s", line);
        }
        Output("=========================================");
    }

    @Override
    public void report(String level, String... message) {
        if (level.equals("result")) {
            return;
        }
        Output("=========================================");
        Output("%s in %s:", level, this.message);
        for (String line : message) {
            Output("  %s", line);
        }
        Output("=========================================");
    }

}

