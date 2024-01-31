public class PredicateTest {
    int a;
    int b;

    public void test() {

    }


    public void main() {


        //needs write for b and read for a
        int c = a + b;
        int d = c + b;
        b = a + c;

        //needs read for b
        if(d > b) {
            //needs read for a and b
            if(a < b){
                //needs write for a and read for b
                int e = a + b;
                a = e + b;
            }
        }

        //needs read for b
        for (int i = 0; i < b; i++) {
            a = b + 1;
        }
    }
}

