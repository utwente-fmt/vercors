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
            myLoop2: myLoop3: myLoop4: while(true) {
                break myLoop2;
                switch(3) {
                    default:
                        break myLoop3;
                    case 3:
                        break;
                }
                x = 3;
            }
            //@ assert x == 3;
            break;
        }
    }
}