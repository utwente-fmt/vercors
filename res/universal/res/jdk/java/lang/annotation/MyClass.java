package java.lang.annotation;

// TODO: Turn this into *
import java.lang.annotation.RetentionPolicy;

class MyClass {
    void m() {
        RetentionPolicy rp = RetentionPolicy.SOURCE;
        //@ assert rp != null;
    }
}