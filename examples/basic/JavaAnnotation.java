import java.lang.Deprecated;

@interface MyAnnotation {
    int value() default 15;
    int foo() default 3;
    boolean bar() default true;
}

@MyAnnotation
class C {
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
}