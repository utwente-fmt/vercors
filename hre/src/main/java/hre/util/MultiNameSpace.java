package hre.util;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.NoSuchElementException;

/**
 * A name space with frame control and multiple definitions per name.
 *
 * @param <Key>
 * @param <Data>
 * @author sccblom
 */
public class MultiNameSpace<Key, Data> implements FrameControl {

    private Map<Key, List<Data>> map = new HashMap<Key, List<Data>>();
    private List<Map<Key, List<Data>>> stack = null;

    public void enter() {
        stack = new List<Map<Key, List<Data>>>(map, stack);
        map = new HashMap<Key, List<Data>>();
    }

    public Iterator<Data> lookup(Key k) {
        return new KeyIterator(k);
    }

    public void add(Key k, Data d) {
        List<Data> l = map.get(k);
        l = new List<Data>(d, l);
        map.put(k, l);
    }

    public void leave() {
        map = stack.item;
        stack = stack.next;
    }

    private final class List<D> {
        final D item;
        final List<D> next;

        public List(D item, List<D> next) {
            this.item = item;
            this.next = next;
        }
    }

    private final class KeyIterator implements Iterator<Data> {
        Key k;
        List<Data> list;
        List<Map<Key, List<Data>>> next;

        KeyIterator(Key k) {
            this.k = k;
            next = new List<Map<Key, List<Data>>>(map, stack);
        }

        public boolean hasNext() {
            for (; ; ) {
                if (list != null) return true;
                if (next == null) return false;
                list = next.item.get(k);
                next = next.next;
            }
        }

        public Data next() {
            if (!hasNext()) {
                throw new NoSuchElementException();
            }
            Data res = list.item;
            list = list.next;
            return res;
        }

        public void remove() {
            throw new UnsupportedOperationException();
        }
    }
}
