final class MyClass {
    void foo() {
        int x = 0;
        my_if: if (true) {
            x = 10;
            break my_if;
            x = 11;
        }
        //@ assert x == 10;
        while(true) {
            myLoop2: while(true) {
                break myLoop2;
                switch(false) {
                    case 3:
                        break;
                    default:
                        break myLoop2;
                }
                x = 3;
            }
            //@ assert x == 3;
            break;
        }
    }
}