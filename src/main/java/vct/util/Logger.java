// -*- tab-width:2 ; indent-tabs-mode:nil -*-
package vct.util;
import java.io.*;

/**
 *
 * @author Stefan Blom, Bert Lisser
 * @version 0.1
 */

public class Logger implements Runnable {

 private DataInput in;
 private PrintStream out;
 private String name;
 private String prefix;

 public Logger(InputStream in,PrintStream out,String name)
 throws IOException
 {
	this.in=new DataInputStream(in);
	this.out=out;
	this.name=name;
	this.prefix=name+": ";
	Thread t=new Thread(this);
	t.setDaemon(true);
	t.start();
 }

 public void run() {
		while(true) {
		try {
			out.println(prefix + in.readLine());
		} catch (EOFException e) {
			return;
		} catch (Exception e) {
			out.println("Logger (" + name + "): "+ e);
		}
	}

 }

}

