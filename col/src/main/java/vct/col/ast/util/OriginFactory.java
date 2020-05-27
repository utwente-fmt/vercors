package vct.col.ast.util;

import hre.ast.Origin;

/**
 * This interface represents a function that produces Origins,
 * given an <E> as argument.
 *
 * @author sccblom
 *
 * @param <E>
 */
public interface OriginFactory<E> {

  /**
   * Extract an origin from the given source.
   * 
   * @param from Object from which an origin can be extracted. 
   * @return Extracted origin.
   */
  public Origin create(E from);

}
