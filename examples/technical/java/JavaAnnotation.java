import java.lang.Deprecated;

@interface MyAnnotation {
    int value() default 15;
    int foo() default 3;
    boolean bar() default true;
}

@interface Dummy {
    ;
}

@MyAnnotation(xxx) // RR: This should preferably not typecheck, but this is currently difficult to accomodate.
class C {
    int xxx;

    @Deprecated
    void m() { }

    @MyAnnotation(100)
    void n() { }

    @MyAnnotation(value = 100, foo = 3, bar = false)
    void o() { }

    @MyAnnotation
    void p() {
        @MyAnnotation int x;
    }

    void xx(@MyAnnotation int param) {

    }

    //@ requires ma != null ** ma instanceof MyAnnotation;
    int x(MyAnnotation ma) {
       return ma.foo();
    }
}