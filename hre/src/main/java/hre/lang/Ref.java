package hre.lang;

/**
 * This class provides a non-atomic reference.
 *
 * @param <T>
 * @author Stefan Blom
 */
public class Ref<T> {

    private T val;

    public Ref() {
        val = null;
    }

    public Ref(T t) {
        val = t;
    }

    public T set(T t) {
        val = t;
        return t;
    }

    public T get() {
        return val;
    }

}
