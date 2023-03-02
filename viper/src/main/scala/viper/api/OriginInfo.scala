package viper.api

import viper.silver.ast.Info

class OriginInfo[O](origin : O, comments : Seq[String]) extends Info {
  
  def this(origin : O) = {
    this(origin,Nil)
  }
  
  val loc=origin
  
  override def comment:Seq[String] = comments
  
  override def toString():String = loc.toString()

  override def isCached: Boolean = false
}

class RefuteInfo[O](origin : O) extends OriginInfo[O](origin,Nil) {
  // This class is used to tag a Not that is the result of
  // encoding "refute x;" as "assert !x;".

}

object Reachable {
  var reachable = scala.collection.mutable.Set[Info]()
  
  var gonogo : viper.api.VerificationControl[Object] = AlwaysGo

}

object VControl {

  def profile(node : viper.silver.ast.Infoed,task : String): Unit = {
    Reachable.gonogo.profile(get_origin(node.info),task)
  }
  
  def get_origin[O](info : viper.silver.ast.Info):O = {
    if (info.isInstanceOf[OriginInfo[O]@unchecked]){
      info.asInstanceOf[OriginInfo[O]].loc
    } else {
      null.asInstanceOf[O]
    }
  }

  var time : Long=0

  def gonogo(fun : viper.silver.ast.Function):Boolean={
    time=System.currentTimeMillis()
    val res=Reachable.gonogo.function(get_origin(fun.info),fun.name)
    Reachable.gonogo.progress("function %s: %s",fun.name,if (res) "Go" else "Skip")
    res
  }
  
  def report(fun : viper.silver.ast.Function,result : Boolean)={
    if (result) {
      Reachable.gonogo.pass(get_origin(fun.info))
    } else {
      Reachable.gonogo.fail(get_origin(fun.info))
    }
    Reachable.gonogo.progress("function %s: %s (%dms)",
        fun.name,
        if (result) "Pass" else "Fail",
        Long.box(System.currentTimeMillis()-time))
  }
  
  def gonogo(fun : viper.silver.ast.Predicate):Boolean={
    time=System.currentTimeMillis()
    val res=Reachable.gonogo.predicate(get_origin(fun.info),fun.name)
    Reachable.gonogo.progress("predicate %s: %s",fun.name,if (res) "Go" else "Skip")
    res
  }
  
  def report(fun : viper.silver.ast.Predicate,result : Boolean)={
    if (result) {
      Reachable.gonogo.pass(get_origin(fun.info))
    } else {
      Reachable.gonogo.fail(get_origin(fun.info))
    }
    Reachable.gonogo.progress("predicate %s: %s (%dms)",
        fun.name,
        if (result) "Pass" else "Fail",
        Long.box(System.currentTimeMillis()-time))
  }
  
  def gonogo(fun : viper.silver.ast.Method):Boolean={
    time=System.currentTimeMillis()
    val res=Reachable.gonogo.method(get_origin(fun.info),fun.name)
    Reachable.gonogo.progress("method %s: %s",fun.name,if (res) "Go" else "Skip")
    res
  }
  
  def report(fun : viper.silver.ast.Method,result : Boolean)={
    if (result) {
      Reachable.gonogo.pass(get_origin(fun.info))
    } else {
      Reachable.gonogo.fail(get_origin(fun.info))
    }
    Reachable.gonogo.progress("method %s: %s (%dms)",
        fun.name,
        if (result) "Pass" else "Fail",
        Long.box(System.currentTimeMillis()-time))
  }
  
}

object AlwaysGo extends viper.api.VerificationControl[Object] {
 def function(x$1: Object,x$2: String): Boolean = true
 def method(x$1: Object,x$2: String): Boolean = true
 def predicate(x$1: Object,x$2: String): Boolean = true
 def fail(x$1: Object): Unit = {}
 def pass(x$1: Object): Unit = {}
 def progress(x$1: String, x$2: Object*): Unit = {}
 def profile(x$1: Object, x$2: String): Unit = {}
 def detail(): Boolean = false
}
