public final class Barrier {
    private int k,n;
    private boolean outgoing;

    /*@
    resource lock_invariant() =
		Perm(k,1) ** Perm(n,read) ** Perm(outgoing,1)
		** 0 <= k
		** k < n
		** (outgoing ? 1 <= k : 0 <= k)
		** n > 1
		;
    @*/

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
            /*@
            loop_invariant Perm(n, read);
            loop_invariant 1<=i && i<= n;
            loop_invariant held(this);
             */
            for (int i=1; i<n; i++) {
                notify();
            }
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
                /*@
                loop_invariant Perm(n, read);
                loop_invariant 1<=i && i<= n;
                loop_invariant held(this);
                 */
                for (int i=1; i<n; i++) {
                    notify();
                }
            }
        }
    }
}