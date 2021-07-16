package hre.util;

/**
 * Maps T1 elements to T2 elements.
 * <p>
 * Can be used for selection under the assumption
 * that null means drop.
 *
 * @param <T1>
 * @param <T2>
 * @author sccblom
 */
public interface Function<T1, T2> {

    public T2 apply(T1 arg);

}
