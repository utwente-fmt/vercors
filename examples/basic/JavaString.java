
class JavaString {
    public static final String CONST = "my constant string";
    public static final String CONST2 = "my constant string";

    // TODO:
    // public static final JavaString js = new JavaString();
    // public static final String CONST = new String();

    void g() {
        "xuz";
        assert "abc" == "abc";
        // assert "abc".data()[0] != "xyz".data()[0]; // TODO: Fix this again, probably in a different way
        assert "abc" != "xyz";

        String xxx;
        String s1 = "aaa";
        String s2 = "bbb";
        String s3 = s1 + s2;

        assert CONST == CONST2;
    }

}
