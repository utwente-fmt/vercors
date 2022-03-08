package hre.lang;

public enum LogLevel {
    Silent(0, null),

    Abort(1, "[abort] "),                   // Internal VerCors Error
    Result(2, ""),                          // The final verdict
    Warning(3, "[warning] "),               // Warnings
    Info(4, ""),                            // User info
    Progress(5, "[progress] "),             // Progress info
    Debug(6, "[debug] "),                   // VerCors development info

    All(Integer.MAX_VALUE, "[filtered] ");  // Only visible when verbosity is set to all

    private final int order;
    private final String shorthand;

    LogLevel(int order, String shorthand) {
        this.order = order;
        this.shorthand = shorthand;
    }

    public int getOrder() {
        return order;
    }

    public String getShorthand() {
        return shorthand;
    }
}
