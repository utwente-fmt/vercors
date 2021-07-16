package viper.api;

public interface ViperError<Origin> {

    public Origin getOrigin(int i);

    public String getError(int i);

    public int getExtraCount();

    public void add_extra(Origin o, String msg);

}
