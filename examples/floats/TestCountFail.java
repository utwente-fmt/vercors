//:: cases TestCountFail
//:: tools silicon
//:: verdict Fail

class TestCount {
    public void test_count_E1() {
        //@ ghost seq<int> xs = seq<int>{ 1, 2 , 2 , 1 };
        //@ assert \sum({ 0 .. 0 },\vcmp(xs,\vrep(1))) == 0;
        //@ assert \sum({ 0 .. 1 },\vcmp(xs,\vrep(1))) == 1;
        //@ assert \sum({ 0 .. 2 },\vcmp(xs,\vrep(1))) == 1;
        //@ assert \sum({ 0 .. 3 },\vcmp(xs,\vrep(1))) == 1;
        //@ assert \sum({ 0 .. 4 },\vcmp(xs,\vrep(1))) == 1;
    }
}

