// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases TestCount
//:: tools silicon
//:: verdict Pass TestCount.test_count_1 TestCount.test_count_2
//:: verdict Fail TestCount.test_count_E1

class TestCount {

  /*@
    given seq<int> vals;
    context_everywhere array != null
      ** (\forall* int i ; 0 <= i && i < array.length ; Perm(array[i],1\2))
      ** (vals.length==array.length)
      ** (\forall int i ; 0 <= i && i < array.length ; array[i]== vals[i])
      ;
    ensures \result == \sum({0 .. array.length},\vcmp(vals,\vrep(3)));
  @*/
  public int test_count_1(int array[]){
    int res=0;
    int k=0;
    //@ loop_invariant 0 <= k && k <= array.length;
    //@ loop_invariant res == \sum({0 .. k},\vcmp(vals,\vrep(3)));
    while(k<array.length){
      if (array[k]==3){
        res = res + 1;
      }
      k++;
    }
    return res;
  }

  public void test_count_2(){
    //@ seq<int> xs = seq<int>{ 1, 2 , 2 , 3 };
    //@ assert \sum({ 0 .. 0 },\vcmp(xs,\vrep(1))) == 0;
    //@ assert \sum({ 0 .. 1 },\vcmp(xs,\vrep(1))) == 1;
    //@ assert \sum({ 0 .. 2 },\vcmp(xs,\vrep(1))) == 1;
    //@ assert \sum({ 0 .. 3 },\vcmp(xs,\vrep(1))) == 1;
    //@ assert \sum({ 0 .. 4 },\vcmp(xs,\vrep(1))) == 1;
  }
  
  public void test_count_E1(){
    //@ seq<int> xs = seq<int>{ 1, 2 , 2 , 1 };
    //@ assert \sum({ 0 .. 0 },\vcmp(xs,\vrep(1))) == 0;
    //@ assert \sum({ 0 .. 1 },\vcmp(xs,\vrep(1))) == 1;
    //@ assert \sum({ 0 .. 2 },\vcmp(xs,\vrep(1))) == 1;
    //@ assert \sum({ 0 .. 3 },\vcmp(xs,\vrep(1))) == 1;
    //@ assert \sum({ 0 .. 4 },\vcmp(xs,\vrep(1))) == 1;
  }

}

