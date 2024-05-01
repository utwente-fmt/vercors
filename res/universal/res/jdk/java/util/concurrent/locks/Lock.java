package java.util.concurrent.locks;

public interface Lock {
	
	void lock();
	
	boolean tryLock();
	
	boolean unlock();	
}