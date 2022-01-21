

public final class IntegerChannel {

    private boolean transfering;

    private int exchangeValue;

    public IntegerChannel() {
        transfering = true;
    }

    public synchronized void write(int v) {
        while (!transfering) {
            try {
                wait();
            } catch (InterruptedException e) {

            }
        }
        transfering = false;
        exchangeValue = v;
        notify();
    }

    public synchronized int read() {
        while (transfering) {
            try {
                wait();
            } catch (InterruptedException e) {

            }
        }
        transfering = true;
        notify();
        return exchangeValue;
    }
}

