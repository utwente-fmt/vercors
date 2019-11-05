package hre.util;

/**
 * This interfaces models control of frames in stacks.
 * @author sccblom
 *
 */
public interface FrameControl {
  /**
   * Enter a new stack frame.
   * When an objects implements this interface, it should
   * store its current value on an internal stack. The value
   * can be either kept or reset to some initial value.
   */
  public void enter();
  /**
   * Leave a stack frame.
   * 
   * Restore the previous state of the object.
   */
  public void leave();
}
