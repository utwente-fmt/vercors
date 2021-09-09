package vct.col.ast.util;

import java.util.Arrays;

import static hre.lang.System.Abort;

public class ClassName {

  public final String name[];
  
  public ClassName(String ... name){
    if (name.length==0) Abort("empty name");
    this.name=Arrays.copyOf(name,name.length);
  }
  
  public ClassName(String[] fullName, String name2) {
    if (fullName==null){
      name=new String[]{name2};
      return;
    }
    name=new String[fullName.length+1];
    for(int i=0;i<fullName.length;i++) name[i]=fullName[i];
    name[fullName.length]=name2;
  }

  public ClassName(ClassName package_name, String name) {
    this(package_name==null?null:package_name.name,name);
  }

  public static boolean equal(String name1[],String name2[]){
    if (name1.length!=name2.length) return false;
    for(int i=0;i<name1.length;i++){
      if (!name1[i].equals(name2[i])) return false;
    }
    return true;
  }

  public String toString() {
    return toString(".");
  }
  public String toString(String separator){
    return toString(name,separator); 
  }
  
  public static String toString(String[] name,String separator) {
    StringBuilder builder=new StringBuilder();
    builder.append(name[0]);
    for(int i=1;i<name.length;i++){
      builder.append(separator);
      builder.append(name[i]);
    }
    return builder.toString();
  }
  
  public static String[] copy(String name[]){
    return Arrays.copyOf(name,name.length);
  }
  
  public int hashCode(){
    return toString(".").hashCode();
  }
  public boolean equals(Object o){
    if (o instanceof ClassName){
      return equal(name,((ClassName)o).name);
    } else {
      return false;
    }
  }

  public ClassName prepend(String[] prefix) {
    if (prefix.length==0) return this;
    String tmp[]=new String[prefix.length+name.length];
    for(int i=0;i<prefix.length;i++){
      tmp[i]=prefix[i];
    }
    for(int i=0;i<name.length;i++){
      tmp[i+prefix.length]=name[i];
    }
    return new ClassName(tmp);
  }
}
