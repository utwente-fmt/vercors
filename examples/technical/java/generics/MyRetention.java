import java.lang.annotation.Retention;

class C {
    Retention r;
}

class D<T> { }

class E {
    void m() {
        D<?> d = null;
    }
}