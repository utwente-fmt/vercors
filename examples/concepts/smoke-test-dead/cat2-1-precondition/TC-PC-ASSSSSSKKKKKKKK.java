//:: cases TC_PC_14_SwitchDeadCase
//:: tools silicon
//:: verdict Fail

class TC_PC_14 {
    //@ requires x > 0;
    void f(int x) {
        switch (x) {
            case 2:
                // dead: x > 0 makes x == -1 impossible
                break;
            case 1: 
                // live: x could be 1
                break;
        }
        //PROBLEM: case 2 will always be dead
    }
}