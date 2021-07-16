package vct.helper

case class AstUnequalException(message: String,left: Any, right: Any, other: Any = None) extends Exception() {
  override def toString: String = {
    var completeMessage = message
    completeMessage += "\n-----------left\n"
    completeMessage += left
    completeMessage += "\n-----------right\n"
    completeMessage += right
    if(other != None){
      completeMessage += "\n-----------other\n"
      completeMessage += other
    }
    completeMessage += "\n-----------\n"
    completeMessage
  }
}
