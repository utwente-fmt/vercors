package vct.col.util;

import java.util.HashSet;
import java.util.Set;

import vct.col.ast.generic.ASTNode;

/**
 * This class manages a set of valid ASTNode classes.
*/
@SuppressWarnings("rawtypes")
public class ASTPermission {
  private Set<Class> allowed_classes=null;
  public void allowAll(){
    allowed_classes=null;
  }
  public void rejectAll(){
    allowed_classes=new HashSet<Class>();
  }
  public void allow(Class c){
    if (allowed_classes==null) rejectAll();
    allowed_classes.add(c);
  }
  public void reject(Class c){
    if (allowed_classes==null) allowAll();
    allowed_classes.remove(c);
  }  
  public void checkPermission(ASTNode n){
    if (allowed_classes==null) return;
    Class c=n.getClass();
    if (allowed_classes.contains(c)) return;
    throw new Error("Invalid argument: "+c.toString());
  }

}
