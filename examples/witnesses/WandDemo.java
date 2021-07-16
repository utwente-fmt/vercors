// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases WandDemo
//:: tools chalice
//:: option --explicit

/**
 * The command line to verify with the VerCors Tool is:
 * <p>
 * vct --chalice --explicit WandDemo.java
 * <p>
 * The expected result is Pass.
 */
class WandDemo {
    int x;

  /*@
    resource readonly()=Perm(x,25);
    resource writeonly()=Perm(x,100);
  @*/

    //@ ensures w:writeonly();
    WandDemo() {
        //@ fold w:writeonly();
    }

    //@ requires r:readonly();
    public /*@ pure */ int get() {
        //@ unfold r:readonly();
        return x;
    }

    /*@
      requires w:writeonly();
      ensures  r:readonly();
      ensures  wand:(readonly()-*writeonly());
    @*/
    void set(int v) {
        //@ unfold w:writeonly();
        x = v;
        //@ fold r:readonly();
    /*@
      create {
        unfold r:readonly();
        use    Perm(this.x,75);
        fold   w:writeonly();
        qed    wand:((r:readonly())-*(w:writeonly()));
      }
    @*/
    }

    void demo() {
        //@ witness wand:(readonly()-*writeonly());
        //@ witness w:writeonly();
        //@ witness r:readonly();
        WandDemo d = new WandDemo() /*@ then { w=w; } @*/;
        int i = 1;
        //@ loop_invariant ww:d.writeonly();
        while (true)
        //@ with { ww = w ; }
        {
            d.set(i) /*@ with { w = ww ; } then { r=r; wand=wand; } */;
            i = (d.get()/*@ with { r=r; } @*/) + (d.get()/*@ with { r=r; } @*/);
            //@ apply wand:((r:d.readonly())-*(ww:d.writeonly()));
        }
    }
}

