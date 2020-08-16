package vct.main;

import vct.logging.PassReport;

public abstract class Pass {
    private String description;

    public String getDescripion(){
        return description;
    }

    public Pass(String description){
        this.description=description;
    }

    public abstract PassReport apply_pass(PassReport report, String... args);
}
