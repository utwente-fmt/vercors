package hre.util;

import java.util.Stack;

/**
 * A stack of references with frame control.
 * 
 * @author sccblom
 *
 * @param <T>
 */
public class FrameReference<T> implements FrameControl {

  private Stack<T> stack=new Stack<T>();
  private T current=null;
  
  public void enter() {
    stack.push(current);
  }

  public void leave() {
    current=stack.pop();
  }

  public void set(T val){
    current=val;
  }
  
  public T get(){
    return current;
  }

}
