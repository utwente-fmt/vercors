

public class IntegerChannel {

    private boolean transfering;

    private int exchangeValue;

    public IntegerChannel() {
        transfering = true;
    }

    public synchronized void writeValue(int v) {
        /*@
            loop_invariant Perm(transfering, 1) ** Perm(exchangeValue,1);
		    loop_invariant held(this);
         @*/
        while (!transfering) {
            try {
                wait();
            } catch (InterruptedException e) {

            }
        }
        transfering = false;
        exchangeValue = v;
        notifyAll();
    }

    public synchronized int readValue() {
        /*@
            loop_invariant Perm(transfering, 1) ** Perm(exchangeValue,1);
            loop_invariant held(this);
         */
        while (transfering) {
            try {
                wait();
            } catch (InterruptedException e) {

            }
        }
        transfering = true;
        notifyAll();
        return exchangeValue;
    }
}

