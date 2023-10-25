package vct.parsers.transform.systemctocol.util;

import scala.collection.immutable.Seq;
import vct.col.origin.Origin;
import vct.col.origin.OriginContent;
import vct.col.origin.VerificationFailure;

public class GeneratedOrigin extends Origin {

    public GeneratedOrigin(Seq<OriginContent> originContents) {
        super(originContents);
    }

    public String preferredName() {
        return "";
    }

    public String context() {
        return "Automatically generated";
    }


    public String inlineContext() {
        return "";
    }


    public String shortPosition() {
        return "";
    }

    @Override
    public String messageInContext(String message) {
        return super.messageInContext(message);
    }

    @Override
    public void blame(VerificationFailure error) {
        super.blame(error);
    }
}
