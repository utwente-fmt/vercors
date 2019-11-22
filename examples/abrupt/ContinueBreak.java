class CB {
    boolean p();

    void foo() {
        while (p()) {
            continue;
            break;
        }
    }
}