package puptol;

import java.io.FileReader;
import java.io.PrintStream;
import java.io.Reader;
import java.util.ArrayList;
import java.util.HashMap;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.google.gson.JsonPrimitive;

import static hre.lang.System.*;

class Group {
  final String Type="group";
  final String ToolName;
  String Title;
  ArrayList<Object> Scenarios=new ArrayList<Object>();
  
  Group(String toolname,String title){
    ToolName=toolname;
    Title=title;
  }
}

class InputParameter {
  final String ParamName="input";
  final String Type = "SHARED";
  final String Value;
  
  public InputParameter(String name){
    Value=name;
  }

}

class Parameter {
  final String ParamName;
  final String Value;
  
  public Parameter(String name,String value){
    ParamName=name;
    Value=value;
  }
}

  class Experiment {
    final String ToolName;
    final String Title;
    final ArrayList<Object> Params=new ArrayList<Object>();
    
    Experiment(String toolname,String title,String backend,String filename,ArrayList<String> args){
      ToolName=toolname;
      Title=title;
      Params.add(new InputParameter(filename));
      int pos=filename.lastIndexOf('.');
      String lang=filename.substring(pos+1);
      Params.add(new Parameter("lang",lang));
      switch(backend){
      case "silicon":
        Params.add(new Parameter("backend","--silver=silicon"));
        break;
      default:
        Warning("cannot map backend %s", backend);
      }
      boolean histcheck=false;
      for (String arg:args){
        switch(arg){
        case "--check-history":
          histcheck=true;
          Params.add(new Parameter("histcheck",arg));
          break;
        default:
          Warning("cannot map argument %s", arg);
        }
      }
      if (!histcheck){
        Params.add(new Parameter("histcheck",""));
      }
    }
  }

public class PuptolConfig {
  
  private static ArrayList<Object> find_group(String toolname,ArrayList<Object> list,String group) {
    Group g=null;
    for(Object o:list){
      if (o instanceof Group){
        g=(Group)o;
        if (g.Title.equals(group)){
          return g.Scenarios;
        }
      }
    }
    g=new Group(toolname,group);
    list.add(g);
    return g.Scenarios;
  }
  private HashMap<String,ArrayList<Object>> updates=new HashMap<String,ArrayList<Object>>();
  
  public void add(String experiment, ArrayList<String> path, String name,
      String tool, String filename, ArrayList<String> options) {
    ArrayList<Object> scenarios=updates.get(experiment);
    if (scenarios==null){
      scenarios=new ArrayList<Object>();
      updates.put(experiment,scenarios);
    }
    String prefix=experiment;
    for(String group:path){
      prefix+="/"+group;
      scenarios=find_group(experiment,scenarios,group);
    }
    filename=prefix+"/"+filename;
    Experiment scen=new Experiment(experiment,name+"/"+tool,tool,filename,options);
    scenarios.add(scen);
  }
  
  public void update(String configfile){
    Progress("updating puptol config %s",configfile);
    try {
      GsonBuilder builder = new GsonBuilder();
      builder.setPrettyPrinting().serializeNulls();
      Gson gson = builder.create();
      JsonParser parser = new JsonParser();
      Reader r=new FileReader(configfile);
      JsonElement element = parser.parse(r);
      JsonObject jobj=(JsonObject)element;
      JsonArray exps=(JsonArray)jobj.get("Experiments");
      for(String key:updates.keySet()){
        boolean found=false;
        JsonElement array=parser.parse(gson.toJson(updates.get(key)));
        for(JsonElement e:exps){
          jobj=(JsonObject)e;
          String label=((JsonPrimitive)jobj.get("Name")).getAsString();
          Debug("entry %s",label);
          if (label.equals(key)){
            found=true;
            Debug("updating %s",label);
            jobj.add("Scenarios",array);
          }
        }
        if (!found){
          Warning("entry for %s not found",key);
        }
      }
      PrintStream out=new PrintStream(configfile);
      out.printf("%s%n",gson.toJson(element));
      out.close();
    } catch (Exception e){
      DebugException(e);
    }
  }

}
