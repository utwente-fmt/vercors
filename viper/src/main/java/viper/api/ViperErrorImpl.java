package viper.api;

import java.util.ArrayList;

public class ViperErrorImpl<Origin> implements ViperError<Origin>{
  
  private ArrayList<Origin> where=new ArrayList<Origin>();
  private ArrayList<String> what=new ArrayList<String>();
  
  
  public ViperErrorImpl(Origin where,String what){
    this.where.add(where);
    this.what.add(what);
  }
  
  public ViperErrorImpl(String what){
    this.where.add(null);
    this.what.add(what);
  }
  
  public Origin getOrigin(int i){
    return where.get(i);
  }
  
  public String getError(int i){
    return what.get(i);
  }
  
  public int getExtraCount(){
    return where.size();
  }

  public void add_extra(String msg) {
    where.add(null);
    what.add(msg);
  }
  
  @Override
  public void add_extra(Origin o, String msg) {
    where.add(o);
    what.add(msg);
  }
  
  public String toString(){
    Origin o=where.get(0);
    String msg=what.get(0);
    if (o!=null){
      return o+": "+msg; 
    } else {
      return msg;
    }
  }
}
