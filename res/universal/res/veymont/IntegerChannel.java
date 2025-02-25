public final class Channel {
    private boolean transfering;

    private MessageType exchangeValue;

    public Channel() {
        transfering = true;
    }

    public synchronized void writeValue(MessageType v) {
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

    public synchronized MessageType readValue() {
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

