package hre.util;

/**
 * Encoding of a filtering predicate.
 * 
 * @author sccblom
 *
 * @param <E>
 */
public interface Filter<E> {

  public boolean pass(E e);

}
