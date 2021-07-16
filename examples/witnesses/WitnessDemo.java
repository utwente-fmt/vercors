// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases WitnessDemo
//:: tools
//:: options --explicit

/**
 * The command line to verify with the VerCors Tool is:
 * <p>
 * vct --chalice --explicit WitnessDemo.java
 * <p>
 * The expected result is Pass.
 */
class WandDemo {
    int x;

  /*@
    resource readonly(frac p)=Perm(x,p/2);
    resource writeonly()=Perm(x,100);
  @*/

    //@ ensures rw:writeonly();
    WandDemo() {
        //@ fold rw:writeonly();
    }

    //@ given frac p;
    //@ requires ro:readonly(p);
    public /*@ pure */ int get() {
        //@ unfold ro:readonly(p);
        return x;
    }

    /*@
      requires rw:writeonly();
      ensures  ro:readonly(100);
      ensures  wand:((rdp:readonly(100)) -* wrp:writeonly());
    @*/
    void set(int v) {
        //@ unfold rw:writeonly();
        x = v;
        //@ fold ro:readonly(100);
    /*@
      create {
        unfold rdp:readonly(100);
        use    Perm(this.x,50);
        fold   wrp:writeonly();
        qed    wand:((rdp:readonly(100))-*wrp:writeonly());
      }
    @*/
    }

    void demo() {
        //@ witness wand:((rdp:readonly(100))-*wrp:writeonly());
        //@ witness wrp:writeonly();
        WandDemo d = new WandDemo() /*@ then { wrp=rw; } */;
        int i = 1;
        //@ loop_invariant inv:d.writeonly();
        //@ inv=wrp;
        while (true) {
            set_call:
            d.set(i) /*@ with { rw=inv; } */;
            //@ wand=set_call.wand;
            int tmp1 = d.get()/*@ with { p = 100 ; ro=set_call.ro; } */;
            int tmp2 = d.get()/*@ with { p = 100 ; ro=set_call.ro; } */;
            i = tmp1 + tmp2;
            //@ apply wand:((rdp:readonly(100))-*wrp:writeonly()) label dummy with { rdp=set_call.ro; };
            //@ inv=dummy.wrp;
        }
    }
}

