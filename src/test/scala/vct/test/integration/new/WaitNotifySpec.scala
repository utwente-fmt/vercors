package integration.`new`

import vct.test.integration.helper.VercorsSpec

class WaitNotifySpec extends VercorsSpec {
  vercors should verify using silicon examples("concepts/waitnotify/Main.pvl", "concepts/waitnotify/Queue.pvl", "concepts/waitnotify/Worker.pvl")
}
