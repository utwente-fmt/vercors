package hre.config;

import java.util.ArrayList;
import java.util.Iterator;

import static hre.lang.System.Output;

/**
 * This class represents a configuration setting whose value is a list of strings.
 * 
 *  
 * @author Stefan Blom
 *
 */
public class StringListSetting implements Iterable<String> {

  /**
   * Create a new string list setting with a default value.
   * @param default_value
   */
  public StringListSetting(String ... default_value){
    for(String s:default_value){
      list.add(s);
    }
  }
  
  /**
   * Boolean that denotes if the default value has been overridden before.
   */
  private boolean override=false; 
  
  /**
   * The list of strings that is to be configured
   */
  private ArrayList<String> list=new ArrayList<String>();
  
  /**
   * Create an option that appends string to this list.
   * @param help Help message.
   * @return Option.
   */
  public Option getAppendOption(String help){
    return new AppendOption(help);
  }
  
  /**
   * Allow iteration over the configured list of strings.
   */
  @Override
  public Iterator<String> iterator() {
    return list.iterator();
  }
  
  /**
   * Option that appends strings to this setting. 
   * @author Stefan Blom
   *
   */
  private class AppendOption extends AbstractOption{
    /**
     * Create an option.
     * @param help Help message.
     */
    public AppendOption(String help){
      super(true,true,help);
    }
    
    /**
     * Add strings given as a comma separated list to the option.
     * The first time strings are added, the default value is erased.
     */
    public void pass(String arg){
      used=true;
      if (!override){
        override=true;
        list.clear();
      }
      for(String item:arg.split(",")){
        list.add(item);
      }
    }
  }

  /**
   * Test is an item is present in the list.
   * @param item Item.
   * @return
   */
  public boolean contains(String item) {
    return list.contains(item);
  }
}
