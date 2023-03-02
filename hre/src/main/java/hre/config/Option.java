package hre.config;

public interface Option {

  boolean needsArgument();

  boolean allowsArgument();

  void pass();

  void pass(String arg);

  String getHelp();

  boolean used();

}
