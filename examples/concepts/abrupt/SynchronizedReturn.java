//:: cases SynchronizedReturn
//:: tools silicon
//:: verdict Fail

/*
In this file it is tested if the return statement below is not accidentally encoded using a goto-oblivious encoding,
which would turn the return statement into "inhale false;", since control flow does not proceed to after the return
statement. However, the synchronized keyword requires the intrinsic lock of the class to be unlocked, so in fact other
code _is_ executed after the return statement. Since the method n() steals the held predicate, this synchronized-related
clean-up code should fail in the normal case, but if the goto-oblivious encoding is used this is not the case.
*/

/*[/expect heldFailed:perm]*/

//@ lock_invariant Perm(f, 1);
class C {
    int f;

    //@ ensures committed(this);
    C() {
        //@ commit this;
    }

    //@ requires committed(this);
    synchronized void m() {
        n();
        return;
    }

    /*@
    requires held(this); // Steal the lock
     @*/
    void n() {

    }
}

/*[/end]*/