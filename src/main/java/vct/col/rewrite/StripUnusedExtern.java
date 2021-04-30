package vct.col.rewrite;

import java.util.HashMap;
import java.util.HashSet;

import vct.col.ast.expr.MethodInvokation;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.decl.*;
import vct.col.ast.util.AbstractRewriter;
import vct.col.ast.util.ContractBuilder;
import vct.col.ast.util.RecursiveVisitor;


/**
 * This class strips unused extern procedures and variables from C programs.
 * It assumes that the C program is contained in the class Ref.
 * 
 * @author Stefan Blom
 *
 */
public class StripUnusedExtern extends AbstractRewriter {

  public StripUnusedExtern(ProgramUnit source) {
    super(source);
  }
  
  private static HashMap<String, DeclarationStatement> vars=new HashMap<String, DeclarationStatement>();
  
  private static HashMap<String, Method> externs=new HashMap<String, Method>();

  private static HashSet<String> used_externs=new HashSet<String>();
  
  private static HashSet<String> defined_names=new HashSet<String>();
  
  private class Scanner extends RecursiveVisitor<Object> {

    public Scanner(ProgramUnit source) {
      super(source);
    }
    
    @Override
    public void visit(Method m) {
      defined_names.add(m.name());
      Method ext = externs.get(m.name());
      if (ext != null) {
        if (m.getContract() != null && !m.getContract().isEmpty()){
          Fail("%s: contract must be written for the extern declaration",m.getOrigin());
        }
        defined_names.add(m.name());
      }
      super.visit(m);
    }
    
    @Override
    public void visit(MethodInvokation s){
      Method ext=externs.get(s.method());
      if (ext!=null) {
        used_externs.add(s.method());
      }
      super.visit(s);
    }
    
  }

  private Scanner scanner=new Scanner(source());

  @Override
  public void visit(DeclarationStatement d) {
    if (d.getParent() instanceof ASTClass){
      DeclarationStatement real = vars.get(d.name());
      if (real != d) {
        return;
      }
    }
    super.visit(d);
  }
  @Override
  public void visit(Method m){
    if (m.isValidFlag(ASTFlags.EXTERN)){
      if (!used_externs.contains(m.name()) || defined_names.contains(m.name())){
        return;
      }
    } else {
      Method ext=externs.get(m.name());
      if (ext!=null){
        if (currentContractBuilder==null&&ext.getContract()!=null){
          currentContractBuilder=new ContractBuilder();
        }
        rewrite(ext.getContract(),currentContractBuilder);
      }
    }
    super.visit(m);
  }
  @Override
  public ProgramUnit rewriteAll() {
    if (source().find("Ref")==null){
      return source();
    }
    ASTClass src=source().find("Ref");
    for(DeclarationStatement d:src.fields()){
      DeclarationStatement old=vars.get(d.name());
      if (d.isValidFlag(ASTFlags.EXTERN)){
        Debug("extern var %s", d.name());
        if (old != null){
          Fail("double declaration of %s", d.name());
        }
      } else {
        if (old!=null && ! old.isValidFlag(ASTFlags.EXTERN)){
          Fail("double declaration of %s", d.name());
        }
      }
      vars.put(d.name(), d);
    }
    for(Method m:source().find("Ref").methods()){
      if (m.isValidFlag(ASTFlags.EXTERN)){
        externs.put(m.name(), m);
      }
    }
    for(Method m:source().find("Ref").methods()){
      if (!m.isValidFlag(ASTFlags.EXTERN)){
        m.accept(scanner);
      }
    }
    ProgramUnit res=super.rewriteAll();
    for(Method m:res.find("Ref").methods()){
      m.clearFlag(ASTFlags.EXTERN);
    }
    for(ASTNode m:res.find("Ref").fields()){
      m.clearFlag(ASTFlags.EXTERN);
    }
    return res;
  }
}
