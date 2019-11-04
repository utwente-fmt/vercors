package viper.api;

public interface VerificationControl<O> {

  boolean function(O origin,String name);
  
  boolean predicate(O origin,String name);
  
  boolean method(O origin,String name);
  
  void pass(O origin);
  
  void fail(O origin);
  
  void progress(String fmt,Object ... args);

  void profile(O o, String task);
  
  boolean detail();
    
}
