package hre.config;

public interface Option {

  public boolean needsArgument();

  public boolean allowsArgument();

  public void pass();

  public void pass(String arg);

  public String getHelp();

  public boolean used();

}
