package hre.ast;

import java.util.ArrayList;

public class BranchOrigin extends WrappingOrigin {

  public final String branch;
  public final Origin base;
  
  @Override
  public WrappingOrigin wrap(Origin other){
    return new BranchOrigin(branch,other);
  }
  
  public BranchOrigin(String branch,Origin base){
    this.branch=branch;
    this.base=base;
  }
  
  @Override
  public void report(String level, Iterable<String> message) {
    ArrayList<String> tmp=new ArrayList<String>();
    tmp.add(String.format("in branch %s:", branch));
    for(String line:message){
      tmp.add(line);
    }
    base.report(level,tmp);
  }

  @Override
  public void report(String level, String... message) {
    ArrayList<String> tmp=new ArrayList<String>();
    tmp.add(String.format("in branch %s:", branch));
    for(String line:message){
      tmp.add(line);
    }
    base.report(level,tmp);
  }

  @Override
  public String toString(){
    return String.format("in branch %s of %s",branch,base);
  }
}
