package hre.tools;

import java.io.IOException;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.lang.reflect.Method;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Comparator;

import static hre.lang.System.Debug;
import static hre.lang.System.DebugException;

/**
 * Command line tool for generating wrapper classes.
 * 
 * This program generates code for a wrapper that assumes that
 * a class is an implementation of the given interface except
 * that it is not a sub-class because it has not been declared
 * or it has been class loaded, or it was compiled against
 * a different version... 
 *  
 * @author Stefan Blom
 *
 */
public class ApiGen {

  public static void main(String args[]) throws IOException{
    hre.lang.System.setOutputStream(System.out, hre.lang.System.LogLevel.Info);
    hre.lang.System.setErrorStream(System.err, hre.lang.System.LogLevel.Info);

    Path currentRelativePath = Paths.get("");
    String s = currentRelativePath.toAbsolutePath().toString();
    Debug("Current relative path is: " + s);
    
    Class<?> c;
    try {
      c = Class.forName(args[0]);
      PrintWriter out;
      if (args.length>1){
        out=new PrintWriter(args[1]);
      } else {
        out=hre.lang.System.getLogLevelOutputWriter(hre.lang.System.LogLevel.Info);
      }
      api_gen(out,c);
      if (args.length>1){
        out.close();
      }
    } catch (ClassNotFoundException e) {
      DebugException(e);
    }
  }
  
  private static String[] listArgument(Type t){
    String tmp=t.toString();
    if (tmp.startsWith("java.util.List")){
      tmp=tmp.replaceFirst(".*<","");
      tmp=tmp.replaceFirst(">.*","");
      return tmp.split("[.]");
    } else {
      return null;
    }
  }
  private static String show(Type t){
    String res=t.toString();
    if (res.startsWith("class ")){
      res=res.substring(6);
    } else if (res.startsWith("interface ")){
      res=res.substring(10);
    }
    return res;
  }

  private static void api_gen(PrintWriter out,Class<?> cl) throws IOException {
    //PrintStream out=new PrintStream(String.format("src/test/Wrapped%s.java",cl.getSimpleName()));
    out.printf("%s;%n", cl.getPackage());
    out.println("import java.lang.reflect.*;");
    out.println();
    out.printf( "/** Wrapper for %s implementations using reflection.  %n",cl.getSimpleName());
    out.println(" *  Thus it can wrap both older and newer versions without linker errors.");
    out.println(" *  This class is generated code! Do not modify!");
    out.println(" */");
    TypeVariable<?> pars[]=cl.getTypeParameters();
    Method methods[]=cl.getMethods();
    java.util.Arrays.sort(methods,new Comparator<Method>(){
      @Override
      public int compare(Method o1, Method o2) {
        int tmp=o1.getName().compareTo(o2.getName());
        if (tmp!=0) return tmp;
        Type t1[]=o1.getParameterTypes();
        Type t2[]=o1.getParameterTypes();
        if (t1.length!=t2.length) return t1.length-t2.length;
        for(int i=0;i<t1.length;i++){
          String s1=t1[i].toString();
          String s2=t2[i].toString();
          tmp=s1.compareTo(s2);
          if (tmp!=0) return tmp;
        }
        return 0;
      }
    });
    if (pars.length>0){
      out.printf("public class Wrapped%s", cl.getSimpleName());
      for(int i=0;i<pars.length;i++){
        out.printf("%s%s",i==0?"<":",",pars[i].getName());
        Type bounds[]=pars[i].getBounds();
        for (int j=0;j<bounds.length;j++){
          String res[]=bounds[j].toString().split(" ");
          out.printf("%s%s",j==0?" extends ":" & ",res[res.length-1]);
        }
      }      
      out.printf("> implements %s", cl.getSimpleName());
      for(int i=0;i<pars.length;i++){
        out.printf("%s%s",i==0?"<":",",pars[i].getName());
      }
      out.println("> {");
    } else {
      out.printf("class Wrapped%s implements %s {%n",
          cl.getSimpleName(),cl.getSimpleName());
    }
    for (int i=0;i<methods.length;i++){
      out.printf("private Method m%d;%n",i);
    }
    //out.printf("private final Class cl=%s.class;%n",cl.getName());
    out.println("private final Object obj;");
    out.printf("public Wrapped%s(Object obj){%n",cl.getSimpleName());
    out.println("  this.obj=obj;");
    out.println("  Class cl=obj.getClass();");
    for (int i=0;i<methods.length;i++){
      Type tt[]=methods[i].getParameterTypes();
      out.printf("  try {%n");
      out.printf("    m%d=cl.getMethod(\"%s\"",i,methods[i].getName());
      for(int j=0;j<tt.length;j++){
        String tmp[]=tt[j].toString().split(" ");
        String tcl=tmp[tmp.length-1];
        out.printf(",%s.class",tcl);
      }
      out.println(");");
      out.printf("  } catch (NoSuchMethodException e) {%n");
      out.printf("    throw new Error(\"NoSuchMethodException: \"+e.getMessage());%n");
      out.printf("  } catch (SecurityException e) {%n");
      out.printf("    throw new Error(\"SecurityException: \"+e.getMessage());%n");
      out.printf("  }%n");
    }
    out.println("}");
    for(int count=0;count<methods.length;count++){
      Method m=methods[count];
      boolean is_void=m.getGenericReturnType().toString().equals("void");
      Type t[]=m.getGenericParameterTypes();
      String list[]=listArgument(m.getGenericReturnType());
      out.printf("/* %s / %s */%n", m.getGenericReturnType(),list);
      String et=null;
      if (list!=null){
        et="";
        for(int i=0;i<list.length-1;i++){
          et+=list[i]+".";
        }
        et+="Wrapped"+list[list.length-1];
        et=et.replaceFirst("[?] extends ","");
      }
      String rt=show(m.getGenericReturnType());
      out.printf("public %s %s(",rt,m.getName());
      for(int i=0;i<t.length;i++){
        out.printf("%s%s arg%d",i==0?"":",",show(t[i]),i);        
      }
      out.println("){");
      out.println("  try {");
      if (!is_void){
        if (list==null){
          out.printf("    return (%s)m%d.invoke(obj",show(m.getGenericReturnType()),count);
        } else {
          out.printf("    java.util.List tmp=(java.util.List)m%d.invoke(obj",count);
        }
      } else {
        out.printf("    m%d.invoke(obj",count);
      }
      for(int i=0;i<t.length;i++){
        out.printf(",arg%d", i);
      }
      out.println(");");
      if (list!=null){
        out.printf("    java.util.List<%s> res=new java.util.ArrayList();%n",et);
        out.printf("    for(Object o : tmp){%n");
        out.printf("      res.add(new %s(o));%n",et);
        out.printf("    }%n");
        out.printf("    return res;%n");
      }
      out.println("  } catch (IllegalAccessException | IllegalArgumentException e) {");
      out.println("    throw new Error(e.getClass()+\" \"+e.getMessage());");
      out.println("  } catch (InvocationTargetException e) {");
      out.println("    e.getCause().printStackTrace();");
      out.println("    throw new Error(\"in reflected call: \"+e.getCause().getClass()+\": \"+e.getCause().getMessage());");
      out.println("  }");

      out.println("}");
    }
    out.println("}");
    //out.close();
  }

  
}
