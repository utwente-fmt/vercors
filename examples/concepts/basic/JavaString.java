class JavaString {
    public static final String CONST = "my constant string";
    public static final String CONST2 = "my constant string";

    // TODO:
    // public static final JavaString js = new JavaString();
    // public static final String CONST = new String();

    void g() {
        "xuz";
        assert "abc" == "abc";
        // assert "abc".data()[0] != "xyz".data()[0]; // Needed for viper 22.02. Unfortunately spec string doesn't have indexing yet
         assert "abc" != "xyz"; // Does not verify yet as the opaque string type does not have indexing yet. Verifies with viper master.

        String xxx;
        String s1 = "aaa";
        String s2 = "bbb";
        String s3 = s1 + s2;

        assert "".isEmpty() || !"".isEmpty();

        assert CONST == CONST2;
    }

}
