package vct.col.util;

import java.util.Set;

import vct.col.ast.stmt.decl.ASTClass;
import vct.col.ast.util.ASTFrame;
import vct.col.ast.stmt.decl.Method;
import vct.col.ast.util.RecursiveVisitor;
import vct.util.ClassName;

/**
 * Scan abstract syntax trees for predicate definitions.
 * 
 * @author Stefan Blom
 *
 */
public class PredicateScanner extends RecursiveVisitor<Object> {

  /**
   * Set of predicates.
   */
  private Set<ClassName> predicates;

  /**
   * Create a scanner.
   * @param shared Shared AST frame for scanning.
   * @param predicates Set into which a ClassName is deposited for every predicate found.
   */
  public PredicateScanner(ASTFrame<Object> shared,Set<ClassName> predicates) {
    super(shared);
    this.predicates=predicates;
  }

  /**
   * Create a scanner.
   * 
   * @deprecated because it is better to know the source through a shared stack.
   * @param predicates Set into which a ClassName is deposited for every predicate found.
   */
  @Deprecated
  public PredicateScanner(Set<ClassName> predicates) {
    super(null,null);
    this.predicates=predicates;
  }
  
  /**
   * Deposit a class name for every predicate.
   */
  @Override
  public void visit(Method m){
    if (m.kind==Method.Kind.Predicate){
      ASTClass cl=(ASTClass)m.getParent();
      ClassName name=new ClassName(cl.getFullName(),m.getName());
      predicates.add(name);
    }
  }

}
