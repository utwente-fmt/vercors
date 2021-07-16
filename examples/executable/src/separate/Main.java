// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases Separate

//:: cases SeparateMain
//:: tools silicon
//:: verdict Pass

package separate;

public class Main {

    public static void main(String[] args) {
        int x = 4;
        int y = Util.incr(x);
        assert y == 5;
    }

}
