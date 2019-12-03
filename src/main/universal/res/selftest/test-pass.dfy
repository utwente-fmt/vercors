class MyClass {
    method incr(x : int) returns (y:int)
        requires x > 0;
        ensures y==x+481;
    {
        y := x+481;
   }
}
