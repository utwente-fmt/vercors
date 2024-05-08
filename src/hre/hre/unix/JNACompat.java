package hre.unix;

import java.lang.reflect.Field;

import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.internal.Cleaner;

public class JNACompat {
	public static <T extends Library> T loadAndNeverClean(String name, Class<T> interfaceClass)
	{
		T result = Native.load(name, interfaceClass);
		
		try	{
			Cleaner cleaner = Cleaner.getCleaner();
	        Field threadField = Cleaner.class.getDeclaredField("cleanerThread");
	        threadField.setAccessible(true);
	        Thread thread = (Thread)threadField.get(cleaner);
	        thread.interrupt();
	        thread.join();
	    } catch(NoSuchFieldException | IllegalAccessException | InterruptedException e) {

	    }
        
        return result;
	}
}