public class Barrier {
    private int k,n;
    private boolean outgoing;

    /*@
        requires nrThreads > 1;
    @*/
    public Barrier(int nrThreads) {
        n = nrThreads;
        k = 0;
        outgoing = false;
    }

    public synchronized void await() {
        /*@
        loop_invariant held(this)
                ** Perm(k,1) ** Perm(n,read) ** Perm(outgoing,1)
                ** k < n
		** (outgoing ? 1 <= k : 0 <= k)
                ** n > 1
        ;
        @*/
        while (outgoing)
        {
            try {
                wait();
            } catch (InterruptedException e) {

            }
        }

        k++;
        if (k == n) {
            outgoing = true;
            k--;
            notifyAll();
        } else {
            /*@
            loop_invariant held(this)
                    ** Perm(k,1) ** Perm(n,read) ** Perm(outgoing,1)
                    ** k < n
			** (outgoing ? 1 <= k : 0 <= k)
                    ** n > 1
            ;
            @*/
            while (!outgoing)
            {
                try {
                    wait();
                } catch (InterruptedException e) {

                }
            }
            k--;
            if (k == 0) {
                outgoing = false;
                notifyAll();
            }
        }
    }
}