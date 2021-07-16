package vct.logging;

import hre.ast.Origin;

public class VerificationResult extends AbstractMessage {

    public final Origin origin;

    public final boolean pass;

    public VerificationResult(boolean pass, Origin origin) {
        this.origin = origin;
        this.pass = pass;
        fatal = !pass;
    }

    @Override
    public void accept(MessageVisitor visitor) {
        visitor.visit(this);
    }

    @Override
    public String toString() {
        return (pass ? "Pass" : "Fail") + " at " + origin;
    }
}
