package vct.rewrite.rtos

import vct.col.ast.Class
import vct.rewrite.rtos.freertosir.{
  EventGroup,
  ISR,
  MessageBuffer,
  Queue,
  Semaphore,
  StreamBuffer,
  Task,
  Timer,
}

class Transformer[O, N](
    tasks: Seq[Task[O]],
    timers: Seq[Timer[O]],
    isrs: Seq[ISR[O]],
    event_groups: Seq[EventGroup[O]],
    semaphores: Seq[Semaphore[O]],
    queues: Seq[Queue[O]],
    stream_buffers: Seq[StreamBuffer[O]],
    message_buffers: Seq[MessageBuffer[O]],
) {
  def get_encoded_system: Seq[Class[N]] = ???
}
