package vct.rewrite.rtos

import vct.col.ast._

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
      get_timers(main),
      get_isrs(main),
      get_event_groups(main),
      get_semaphores(main),
      get_queues(main),
      get_stream_buffers(main),
      get_message_buffers(main),
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
    calls.filter(i =>
      Utils.get_applicable_name(i.applicable).equals("vesuvTaskCreate")
    ).map(t => Task.of(t, decls))

  private def get_timers[O](main: CFunctionDefinition[O]): Seq[Timer[O]] = ???

  private def get_isrs[O](main: CFunctionDefinition[O]): Seq[ISR[O]] = ???

  private def get_event_groups[O](
      main: CFunctionDefinition[O]
  ): Seq[EventGroup[O]] = ???

  private def get_semaphores[O](
      main: CFunctionDefinition[O]
  ): Seq[Semaphore[O]] = ???

  private def get_queues[O](main: CFunctionDefinition[O]): Seq[Queue[O]] = ???

  private def get_stream_buffers[O](
      main: CFunctionDefinition[O]
  ): Seq[StreamBuffer[O]] = ???

  private def get_message_buffers[O](
      main: CFunctionDefinition[O]
  ): Seq[MessageBuffer[O]] = ???

  private def construct_encoded_system[O, N](
      tasks: Seq[Task[O]],
      timers: Seq[Timer[O]],
      isrs: Seq[ISR[O]],
      event_groups: Seq[EventGroup[O]],
      semaphores: Seq[Semaphore[O]],
      queues: Seq[Queue[O]],
      stream_buffers: Seq[StreamBuffer[O]],
      message_buffers: Seq[MessageBuffer[O]],
  ): Seq[Class[N]] = ???
}
