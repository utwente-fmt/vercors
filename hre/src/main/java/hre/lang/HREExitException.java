package hre.lang;

public class HREExitException extends Error {
  
  /**
   * 
   */
  private static final long serialVersionUID = 1L;
  public final int exit;
  
  public HREExitException(int val){
    super("exit["+val+"]");
    exit=val;
  }

}
