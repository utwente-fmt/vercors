// -*- tab-width:2 ; indent-tabs-mode:nil -*-
package hre.ast;

import java.io.*;
import java.util.Stack;

/**
 * This class provides the pretty printing classes with a way of
 * keeping track of which AST nodes correspond to which output characters.
 *  
 * @author sccblom
 *
 */
public class TrackingOutput {
  public static int num_spaces = 4;

  private PrintWriter output;
//  private boolean show;

  private static class Frame {
    public int line,col;
    TrackingTree tree;
    public Frame(int line,int col,Origin origin){
      this.line=line;
      this.col=col;
      this.tree=new TrackingTree(origin);
    }
  }
  private Stack<Frame> stack=new Stack<Frame>();
  private Frame frame;
  private int line=1;
  private int col=1;
  private int indent=0;
  private boolean atnewline=true;
  private boolean delayed_ghost=false;
  private boolean closeout;
  
  protected int ghost_level;
  
  public void enterGhost(){
    if (ghost_level==0){
      if (delayed_ghost){
        delayed_ghost=false;
      } else {
        println("/*@");
        incrIndent();
      }
    }
    ghost_level++;
  }
  
  public void leaveGhost(){
    ghost_level--;
    if (ghost_level==0){
        delayed_ghost=true;
    }
  }

  public TrackingOutput(PrintWriter output,boolean closeout){
    this.closeout=closeout;
    this.output=output;
    frame=new Frame(line,col,new MessageOrigin("unknown"));
  }

  public void enter(Origin origin){
    stack.push(frame);
    frame=new Frame(line,col,origin);
  }
  public void leave(Origin origin){
    if (stack.empty()){
      throw new Error("attempt to leave outmost frame");
    }
    Frame parent=stack.pop();
    if (origin != frame.tree.getOrigin()){
      throw new Error("enter/leave mismatch: found "+frame.tree.getOrigin()+", expected: "+origin);
    }
    parent.tree.add(frame.tree,frame.line,frame.col,line,col);
    frame=parent;
  }
  public void incrIndent(){ indent+=num_spaces; }
  public void decrIndent(){ 
    if (delayed_ghost){
      delayed_ghost=false;
      indent-=num_spaces;
      println("@*/");
    }
    indent-=4;
  }
  
  
  public void clearline(){
    if(!atnewline) newline();
  }
  
  public void newline(){
    output.append("\n");
    line++;
    atnewline=true;
    col=1;
  }
  public void print(String s){
    if (delayed_ghost){
      indent-=num_spaces;
    }
    if(atnewline){
      for(int i=0;i<indent;i++) {
        output.print(" ");
      }
      atnewline=false;
      col+=indent;
    }
    if (delayed_ghost){
      delayed_ghost=false;
      output.println("@*/");
      for(int i=0;i<indent;i++) {
        output.print(" ");
      }
      col+=indent;
    }
    output.print(s);
    col+=s.length();
  }
  public void println(String s){
    print(s);
    newline();
  }
  public void printf(String format,Object ... args){
    print(String.format(format,args));
  }
  public void lnprintf(String format,Object ... args){
    print(String.format(format,args));
    newline();
  }
  public TrackingTree close(){
    if (!stack.empty()){
      throw new Error("tracking stack not empty");
    }
    if (closeout) output.close();
    return frame.tree;
  }
}

