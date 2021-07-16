package hre.util;

import java.util.Iterator;

/**
 * Factory interface for mapping and selecting iterators.
 *
 * @param <S>
 * @param <T>
 * @author sccblom
 */
public class FilteredIterable<S, T> implements Iterable<T> {

    private Iterable<S> iterable;
    private Function<S, T> filter;

    public FilteredIterable(Iterable<S> iterable, Function<S, T> filter) {
        this.iterable = iterable;
        this.filter = filter;
    }

    public Iterator<T> iterator() {
        return new FilteredIterator<S, T>(iterable.iterator(), filter);
    }

}
