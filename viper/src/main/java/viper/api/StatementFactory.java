package viper.api;

import hre.util.Triple;

import java.util.List;

/**
 * 
 * @author Stefan Blom
 *
 * @param <O> Type of objects that represent origins.
 * @param <T> Type of objects that represent types.
 * @param <E> Type of objects that represent expressions.
 * @param <S> Type of objects that represent statements.
 */
public interface StatementFactory<O, T, E, S> {

  /** Create a statement that inhales a boolean/permission expression. */
  S inhale(O o,E expr);
  
  /** Create a statement that exhales a permission expression. */
  S exhale(O o,E expr);

  /** Create a statement that asserts a boolean/permission expression. */
  S assert_(O o,E expr);

  /** Create a statement that refutes a boolean expression. */
  S refute(O o,E expr);

  S fold(O o,E expr);
  
  S unfold(O o,E expr);
  
  S new_object(O o,E var,List<String> names,List<T>types);

  /** Assign a value to a field or local variable.
   *  Note that Silver uses different assignment statements for
   *  field and locals, but this can be deduced from the
   *  location expression so a single method suffices.
   */
  S assignment(O o,E loc, E val);
  
  /** Create a goto statement. */
  S goto_(O o,String l);
  
  /** Create a target label for a goto statement. */
  S label(O o, String l, List<E> invs);

  /** Create a block of statements. */
  S block(O o,List<S> stats);
  
  /** Create a while loop. */
  S while_loop(O o, E cond, List<E> invs, List<Triple<O,String,T>> locals, S body);
  
  /** Create a method call. */
  S method_call(O o,String name,List<E> args,List<E> outs,List<Triple<O,String,T>> pars,List<Triple<O,String,T>> rets);
  
  /** Create an if-the-else. */
  S if_then_else(O o,E c,S s1,S s2);

}
