package hre.config;

import static hre.lang.System.*;

public abstract class AbstractOption implements Option {

  private final String help;
  private final boolean arg_needed;
  private final boolean arg_allowed;
  public AbstractOption(boolean arg_needed,boolean arg_allowed,String help){
    this.arg_needed=arg_needed;
    this.arg_allowed=arg_allowed;
    this.help=help;
  }
  @Override
  public boolean needsArgument() {
    return arg_needed;
  }

  @Override
  public boolean allowsArgument() {
    return arg_allowed;
  }

  @Override
  public void pass() {
    if (arg_allowed){
      used=true;
    } else {
      Abort("illegal call to pass()");
    }
  }

  @Override
  public void pass(String arg) {
    Abort("illegal call to pass(String)");
  }
  
  public String getHelp(){
    return help;
  }
  
  protected boolean used=false;
  
  public boolean used(){
    return used;
  }

}
