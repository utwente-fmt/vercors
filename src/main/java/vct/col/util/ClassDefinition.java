package vct.col.util;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import static hre.lang.System.Debug;

public class ClassDefinition extends AnyDefinition {

  public final String full_name[];
  public final ClassDefinition parent;
  
  private Map<String,FieldDefinition> field_map=new HashMap<String,FieldDefinition>();
  private Map<String,MethodDefinition> method_map=new HashMap<String,MethodDefinition>();
  private Map<String,ClassDefinition> child_map=new HashMap<String,ClassDefinition>();
  
  public ClassDefinition(){
    super(null);
    parent=null;
    full_name=new String[0];
  }
  
  /** kludge for adding type parameter. */
  public ClassDefinition(String name){
    super(name);
    parent=this;
    full_name=new String[1];
    full_name[0]=name;
  }
  
  private ClassDefinition(String name,ClassDefinition parent){
    super(name);
    this.parent=parent;
    int N=parent.full_name.length;
    full_name=new String[N+1];
    for(int i=0;i<N;i++) full_name[i]=parent.full_name[i];
    full_name[N]=name;
  }
  
  public void addField(String name,boolean is_static){
    FieldDefinition res=field_map.get(name);
    if (res==null){
      res=new FieldDefinition(name,is_static,this);
      field_map.put(name,res);
      Debug("new field %s.%s",this.fullName(),name);
    }
  }

  public void addMethod(String name){
    MethodDefinition res=method_map.get(name);
    if (res==null){
      res=new MethodDefinition(name);
      method_map.put(name,res);
      Debug("new method %s.%s",this.fullName(),name);
    }
  }
  
  public ClassDefinition addNested(String name){
    ClassDefinition res=child_map.get(name);
    if(res==null){
      res=new ClassDefinition(name,this);
      child_map.put(name,res);
      Debug("new package or class %s",res.fullName());
    }
    return res;
  }

  private String fullName() {
    String res=full_name[0];
    for(int i=1;i<full_name.length;i++) res+="."+full_name[i];
    return res;
  }

  public ClassDefinition lookupClass(String[] name) {
    ClassDefinition res=this;
    for(int i=0;i<name.length && res!=null;i++){
      res=res.child_map.get(name[i]);
    }
    return res;
  }

  public Collection<ClassDefinition> getClasses() {
    return child_map.values();
  }

  public Collection<MethodDefinition> getMethods() {
    return method_map.values();
  }
  
  public Collection<FieldDefinition> getFields() {
    return field_map.values();
  }
  
}
