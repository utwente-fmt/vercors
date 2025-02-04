package vct.rewrite.rtos

import vct.col.ast._
import vct.rewrite.rtos.freertosir._

object RTOSEncoder {
  def transform[O, N](
      c_model: Seq[GlobalDeclaration[O]]
  ): Seq[GlobalDeclaration[N]] = {
    val def_methods: Seq[CFunctionDefinition[O]] = c_model.head.collect {
      case f: CFunctionDefinition[O] => f
    }
    val main: CFunctionDefinition[O] = get_main_method(def_methods)
    val calls: Seq[CInvocation[O]] = main.body.collect {
      case call: CInvocation[O] => call
    }
    construct_encoded_system(
      get_tasks(calls, def_methods),
      get_timers(calls, def_methods),
      get_isrs(calls, def_methods),
      get_event_groups(calls),
      get_semaphores(calls),
      get_queues(calls),
      get_stream_buffers(calls),
      get_message_buffers(calls),
    )
  }

  private def get_main_method[O](
      decls: Seq[CFunctionDefinition[O]]
  ): CFunctionDefinition[O] =
    decls.find(f => Utils.get_declarator_name(f.declarator).equals("main"))
      .getOrElse(throw new IllegalStateException("No main method declared!"))

  private def get_tasks[O](
      calls: Seq[CInvocation[O]],
      decls: Seq[CFunctionDefinition[O]],
  ): Seq[Task[O]] =
    Utils.filter_by_name(calls, "vesuvTaskCreate").map(t => Task.of(t, decls))

  private def get_timers[O](calls: Seq[CInvocation[O]],
                            decls: Seq[CFunctionDefinition[O]]): Seq[Timer[O]] =
    Utils.filter_by_name(calls, "vesuvTimerCreate").map(t => Timer.of(t, decls))

  private def get_isrs[O](calls: Seq[CInvocation[O]],
                          decls: Seq[CFunctionDefinition[O]]): Seq[ISR[O]] =
    Utils.filter_by_name(calls, "vesuvISRCreate").map(t => ISR.of(t, decls))

  private def get_event_groups[O](
                                   calls: Seq[CInvocation[O]]
  ): Seq[EventGroup] =
    Utils.filter_by_name(calls, "xEventGroupCreate").map(_ => EventGroup())

  private def get_semaphores[O](
                                 calls: Seq[CInvocation[O]]
  ): Seq[Semaphore] =
    Utils.filter_by_name(calls, "xSemaphoreCreateBinary").map(_ => BinarySemaphore(false)) ++
      Utils.filter_by_name(calls, "xSemaphoreCreateMutex").map(_ => BinarySemaphore(true)) ++
      Utils.filter_by_name(calls, "xSemaphoreCreateRecursiveMutex").map(_ => RecursiveMutex())

  private def get_queues[O](calls: Seq[CInvocation[O]]): Seq[Queue] =
    Utils.filter_by_name(calls, "xQueueCreate").map(v => Queue.of(v))

  private def get_stream_buffers[O](
                                     calls: Seq[CInvocation[O]]
  ): Seq[StreamBuffer] =
    Utils.filter_by_name(calls, "xStreamBufferCreate").map(v => StreamBuffer.of(v))

  private def get_message_buffers[O](
                                      calls: Seq[CInvocation[O]]
  ): Seq[MessageBuffer] =
    Utils.filter_by_name(calls, "xMessageBufferCreate").map(v => MessageBuffer.of(v))

  private def construct_encoded_system[O, N](
      tasks: Seq[Task[O]],
      timers: Seq[Timer[O]],
      isrs: Seq[ISR[O]],
      event_groups: Seq[EventGroup],
      semaphores: Seq[Semaphore],
      queues: Seq[Queue],
      stream_buffers: Seq[StreamBuffer],
      message_buffers: Seq[MessageBuffer],
  ): Seq[Class[N]] = ???
}
