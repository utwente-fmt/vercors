package vct.parsers.transform.systemctocol.util;

import vct.col.origin.Origin;
import vct.col.origin.VerificationFailure;

public class GeneratedOrigin implements Origin {

    @Override
    public String preferredName() {
        return "";
    }

    @Override
    public String context() {
        return "Automatically generated";
    }

    @Override
    public String inlineContext() {
        return "";
    }

    @Override
    public String shortPosition() {
        return "";
    }

    @Override
    public String messageInContext(String message) {
        return Origin.super.messageInContext(message);
    }

    @Override
    public void blame(VerificationFailure error) {
        Origin.super.blame(error);
    }
}
