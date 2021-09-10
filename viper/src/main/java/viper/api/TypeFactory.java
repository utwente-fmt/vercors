package viper.api;

import java.util.Map;

/**
 * Factory API for Silver types.
 * 
 * @author Stefan Blom
 *
 * @param <T> Type of objects that represent types.
 */
public interface TypeFactory<T> {

  /** Create an integer type. */
  T Int();
  
  /** Create a boolean type. */
  T Bool();
  
  /** Create a fractional permission type. */
  T Perm();
  
  /** Create a reference type. */
  T Ref();
  
  /** Create a list type. */
  T List(T t);
  
  /** Create a bag or multi-set type. */
  T Bag(T t);
  
  /** Create a set type. */
  T Set(T t);
  
  /** Create a domain type. */
  T domain_type(String name, Map<String, T> args);
  
  /** Create a type variable. */
  T type_var(String name);

}
