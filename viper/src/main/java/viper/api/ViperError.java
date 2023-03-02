package viper.api;

public interface ViperError<Origin> {
  
  Origin getOrigin(int i);
  
  String getError(int i);
  
  int getExtraCount();
  
  void add_extra(Origin o, String msg);

}
