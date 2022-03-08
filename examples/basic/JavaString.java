
class JavaString {
    public static final String CONST = "my constant string";
    public static final String CONST2 = "my constant string";

    // TODO:
    // public static final JavaString js = new JavaString();
    // public static final String CONST = new String();

    void g() {
        "xuz";
        assert "abc" == "abc";
        //@ assert "abc".data()[0] != "xyz".data()[0]; // ???
        assert "abc" != "xyz";
        assert CONST == CONST2;
    }

}
