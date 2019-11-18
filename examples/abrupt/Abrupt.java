final class MyClass extends Exception {
    void foo() {
        int x = 0;
        while(true) {
            myLoop2: while(true) {
                break myLoop2;
                switch(false) {
                    case 3:
                        break;
                    default:
                        break; // Test case, this one should not be changed
                }
                x = 3;
            }
            //@ assert x == 3;
            break;
        }
    }
}