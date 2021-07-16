package hre.util;

/**
 * Encoding of a filtering predicate.
 *
 * @param <E>
 * @author sccblom
 */
public interface Filter<E> {

    public boolean pass(E e);

}
