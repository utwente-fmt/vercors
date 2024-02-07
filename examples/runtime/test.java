public class PredicateTest {
    int a;
    int b;

    public void test() {

    }


    public void main() {


        PredicateTest test = new PredicateTest();

        //needs write for b and read for a
        int c = test.a + test.b;
        int d = c + test.b;
        test.b = test.a + c;

        //needs read for b
        if(d > test.b) {
            //needs read for a and b
            if(test.a < test.b){
                //needs write for a and read for b
                int e = test.a + test.b;
                test.a = e + test.b;
            }
        }

        //needs read for b
        for (int i = 0; i < test.b; i++) {
            test.a = test.b + 1;
        }
    }
}

