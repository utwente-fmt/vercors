package rise4fun

import com.google.gson._
import java.io.PrintStream
import scala.collection.mutable.ArrayBuffer

class Rise4funConfig() {
  var examples = ArrayBuffer[Example]()
  
  // add an example program to the pool of Rise4fun examples
  def addExample(name:String, path:String) = {
    examples += new Example(name, path);
  }
  
  def toJson() = {
    // initialise a new JSON builder
    var builder = new GsonBuilder();
    builder.setPrettyPrinting().serializeNulls();
    
    // convert and return the examples array to JSON
    builder.create().toJson(new Suite(examples.toArray))
  }
}