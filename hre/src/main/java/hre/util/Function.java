package hre.util;

/**
 * Maps T1 elements to T2 elements.
 * 
 * Can be used for selection under the assumption
 * that null means drop.
 * 
 * @author sccblom
 *
 * @param <T1>
 * @param <T2>
 */
public interface Function<T1, T2> {
  T2 apply(T1 arg);
}
