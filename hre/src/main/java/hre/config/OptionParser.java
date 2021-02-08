package hre.config;

import java.util.*;
import java.util.Map.Entry;

import static hre.lang.System.*;

/**
 * This class implements a simple options parser.
 * 
 * @author Stefan Blom
 */
public class OptionParser {

  /**
   * Mapping from single characters to options.
   */
  private Map<Character,Option> short_options=new Hashtable<Character,Option>();
  
  /**
   * Mapping from names to options.
   */
  private Map<String,Option> long_options=new Hashtable<String,Option>();
  
  /**
   * Mapping from options to names.
   */
  private Map<Option,String> option_list=new LinkedHashMap<Option,String>();
  
  /**
   * Add an option to the parser under one or more names.
   * 
   * @param option The option to be added.
   * @param names List of names for this option. To add a short name, a Character should be used. to add a long name, a String must be used.
   */
  public void add(Option option,Object...names){
    String namelist=null;
    for(Object name:names){
      if(namelist!=null) namelist+=", "; else namelist="";
      if (name instanceof Character){
        namelist+="-"+name;
        short_options.put((Character)name, option);
      } else if (name instanceof String){
        namelist+="--"+name;
        long_options.put((String)name,option);
      } else {
        Fail("illegal options name: %s",name.getClass());
      }
    }
    option_list.put(option,namelist);
  }

  /**
   * Parse an array of arguments.
   * @param args Array of arguments to be parsed.
   * @return The arguments which did not match a registered option.
   */
  public String[] parse(String args[]){
    ArrayList<String> unparsed=new ArrayList<String>();
    for(int i=0;i<args.length;i++){
      if (args[i].equals("--")) {
        for(i++;i<args.length;i++){
          unparsed.add(args[i]);
        }
      } else if (args[i].startsWith("--")){
        String name=args[i].substring(2);
        int arg_idx=name.indexOf('=');
        String arg=null;
        if (arg_idx >= 0) {
          arg=name.substring(arg_idx+1);
          name=name.substring(0,arg_idx);
        }
        Option opt=long_options.get(name);
        Objects.requireNonNull(opt, String.format("unknown option %s", name));
        if (arg==null && opt.needsArgument() && i+1<args.length){
          i++;
          arg=args[i];
        }
        if (arg==null){
          if (opt.needsArgument()){
            Fail("options %s: missing argument",name);
          }
          opt.pass();
        } else {
          if(!opt.allowsArgument()){
            Fail("option %s does not allow an argument",name);
          }
          opt.pass(arg);
        }
      } else if (args[i].startsWith("-")) {
        int N=args[i].length();
        if (N>2){
          Option opt=short_options.get(args[i].charAt(1));
          if (opt !=null && opt.needsArgument()){
            String arg=args[i].substring(2);
            opt.pass(arg);
            continue;
          }
        }
        for(int j=1;j<N;j++){
          Option opt=short_options.get(args[i].charAt(j));
          Objects.requireNonNull(opt, String.format("unknown flag %c", args[i].charAt(j)));
          if (opt.needsArgument()){
            Fail("option %c needs an argument.",args[i].charAt(j));
          }
          opt.pass();
        }
      } else {
        unparsed.add(args[i]);
      }
    }
    return unparsed.toArray(new String[0]);
  }
  
  public Option getHelpOption(){
    return new HelpOption();
  }
  
  private class HelpOption extends AbstractOption {

    public HelpOption() {
      super(false,false,"Prints all options of VerCors");
    }

    public void pass(){
      Output("Options are:");
      for(Entry<Option,String> entry : option_list.entrySet()){
        Output(" %-20s  : %s",entry.getValue(),entry.getKey().getHelp());
      }
      System.exit(0);
    }
  }

}

