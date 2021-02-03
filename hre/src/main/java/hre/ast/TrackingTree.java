// -*- tab-width:2 ; indent-tabs-mode:nil -*-
package hre.ast;

import static hre.lang.System.*;
import java.util.ArrayList;

/**
 * Provides a mapping from positions in a file to Origins.
 * 
 * If the origins of generated code are stored in the data structures
 * then error messages using positions in the generated code can be translated
 * to error messages using the correct origins.
 * 
 * @author sccblom
 *
 */
public class TrackingTree {

  private Origin origin;
  private static class Child {
    public int first_line, first_col, last_line, last_col;
    public TrackingTree node;
    public void print(String prefix){
      Output("%s[%d.%d,%d.%d) -> %s",prefix,first_line, first_col, last_line, last_col,node.origin);
      node.print(prefix+"  ");
    }
  }

  ArrayList<Child> children=new ArrayList<Child>();
  
  public TrackingTree(Origin origin){
    this.origin=origin;
  }
  public Origin getOrigin(){
    return origin;
  }
  public void add(TrackingTree tree,int first_line, int first_col, int last_line, int last_col){
    Child child=new Child();
    child.first_line=first_line;
    child.first_col=first_col;
    child.last_line=last_line;
    child.last_col=last_col;
    child.node=tree;
    children.add(child);
  }

  private static final boolean leq(int line1,int col1,int line2,int col2){
    return (line1<line2) || (line1==line2 && col1<=col2);
  }
  private static final boolean less(int line1,int col1,int line2,int col2){
    return (line1<line2) || (line1==line2 && col1<col2);
  }
  public Origin getOrigin(int line,int col){
    for(int i=0;i<children.size();i++){
      Child child=children.get(i);
      if (leq(child.first_line,child.first_col,line,col) &&
          less(line,col,child.last_line,child.last_col)
      ){
        return child.node.getOrigin(line,col);
      }
    }
    return origin;
  }
  
  public void print(String prefix){
    for(int i=0;i<children.size();i++){
      Child child=children.get(i);
      child.print(prefix);
    } 
  }

}

