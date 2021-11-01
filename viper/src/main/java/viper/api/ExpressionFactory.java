package viper.api;

import hre.util.Triple;

import java.util.List;
import java.util.Map;

/**
 * 
 * @author Stefan Blom
 *
 * @param <O> Type of objects that represent origins.
 * @param <T> Type of objects that represent types.
 * @param <E> Type of objects that represent expressions.
 */
public interface ExpressionFactory<O,T,E> {

  /** Create an integer constant. */
  E Constant(O o, int i);

  /** Create an integer constant. */
  E Constant(O o, float f);

  /** Create an integer constant. */
  E Constant(O o, double d);

  /** Create a boolean constant. */
  E Constant(O o, boolean b);

  /** Create a constant set. */
  E explicit_set(O o,T t,List<E> elems);
  
  /** Create a constant bag. */
  E explicit_bag(O o,T t,List<E> elems);
  
  /** Create a constant sequence. */
  E explicit_seq(O o, T t, List<E> elems);

  /** Create a <code>null</code> constant. */
  E null_(O o);

  E write_perm(O o);
  E read_perm(O o);
  E no_perm(O o);


  E range(O o, E e1, E e2);
  E take(O o, E e1, E e2);
  E drop(O o, E e1, E e2);
  E slice(O o, E e1, E e2, E e3);
  E seq_update(O o, E e1, E e2, E e3);
  E size(O o, E e1);
  E index(O o, E e1, E e2);
  
  E append(O o, E e1, E e2);
  E union(O o, E e1, E e2);
  
  E predicate_call(O o, String name, List<E> args);
  E function_call(O o, String name, List<E> args, T rt);
  E result(O o, T t);
  
  E domain_call(O o, String name, List<E> args, Map<String, T> dpars, T rt, String domain);
 
  E field_access(O o, E e1, E e2);
  E scale_access(O o, E e1, E e2);
  
  E unfolding_in(O o, E e1, E e2);
  
  /** Create a node that accesses a field in an object. */
  E FieldAccess(O o, E obj, String field, T t);
  
  /** Create a node that accesses a local variable.
   *  Note that in Silver arguments and return variables are
   *  considered to be local variables.
   */
  E local_name(O o, String name, T t);

  /** Create an universal quantification. */
  E forall(O o, List<Triple<O, String, T>> vars, List<List<E>> triggers, E e);
  
  /** Create an existential quantification. */
  E exists(O o, List<Triple<O, String, T>> vars, List<List<E>> triggers, E e);
  
  E old(O o, E e1);
  
  /** Create a not equal expression */
  E neq(O o, E e1, E e2);

  E eq(O o, E e1, E e2);

  /** Create a greater than expression. */
  E gt(O o, E e1, E e2);
  
  E lt(O o, E e1, E e2);

  E gte(O o, E e1, E e2);
  
  E lte(O o, E e1, E e2);

  E seq_contains(O o, E e1, E e2);
  E any_set_contains(O o, E e1, E e2);
  E any_set_minus(O o, E e1, E e2);
  E any_set_union(O o, E e1, E e2);
  E any_set_intersection(O o, E e1, E e2);
  E any_set_subset(O o, E e1, E e2);

  E let(O o, String v, T t, E e1, E e2);
  
  E mult(O o, E e1, E e2);
  E floor_div(O o, E e1, E e2);
  E mod(O o, E e1, E e2);
  E div(O o, E e1, E e2);
  E frac(O o, E e1, E e2);
  E add(O o, E e1, E e2);
  E perm_add(O o, E e1, E e2);
  E sub(O o, E e1, E e2);
  E neg(O o, E e1);

  E fp_neg(O o, E e1);
  E fp_add(O o, E e1, E e2);
  E fp_sub(O o, E e1, E e2);
  E fp_mult(O o, E e1, E e2);
  E fp_div(O o, E e1, E e2);
  E fp_eq(O o, E e1, E e2);
  E fp_neq(O o, E e1, E e2);
  E fp_lte(O o, E e1, E e2);
  E fp_gte(O o, E e1, E e2);
  E fp_lt(O o, E e1, E e2);
  E fp_gt(O o, E e1, E e2);
  E int_to_float(O o, E e1);
  E int_to_double(O o, E e1);
  E float_to_double(O o, E e1);
  E double_to_float(O o, E e1);
  E float_in_bounds(O o, E e1);
  E float_not_nan(O o, E e1);

  E and(O o, E e1, E e2);
  E or(O o, E e1, E e2);
  E implies(O o, E e1, E e2);
  E not(O o, E e1);
  
  E cond(O o, E e1, E e2, E e3);

  E current_perm(O o, E expr);

}
