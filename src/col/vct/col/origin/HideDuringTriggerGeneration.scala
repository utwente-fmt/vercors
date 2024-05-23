package vct.col.origin

/** Attached to triggers that should be hidden during trigger generation. That
  * is: if there are only triggers with this attached, we should pretend that
  * there are no triggers yet.
  */
object HideDuringTriggerGeneration extends OriginContent
