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
    val stmts: Seq[Expr[O]] = main.body.collect { case Eval(expr) => expr }

    new Transformer[O, N](
      get_tasks(stmts, def_methods),
      get_timers(stmts, def_methods),
      get_isrs(stmts, def_methods),
      get_event_groups(stmts),
      get_semaphores(stmts),
      get_queues(stmts),
      get_stream_buffers(stmts),
      get_message_buffers(stmts),
    ).get_encoded_system
  }

  private def get_main_method[O](
      decls: Seq[CFunctionDefinition[O]]
  ): CFunctionDefinition[O] =
    decls.find(f => Utils.get_declarator_name(f.declarator).equals("main"))
      .getOrElse(throw new IllegalStateException("No main method declared!"))

  private def get_tasks[O, N](
      stmts: Seq[Expr[O]],
      decls: Seq[CFunctionDefinition[O]],
  ): Seq[Task[O, N]] = {
    Utils.resolve_freertos_constructs[O, N, Task[O, N]](
      stmts,
      "vesuvTaskCreate",
      (v: Option[CLocal[O]], inv: CInvocation[O]) =>
        Task.of[O, N](v, inv, decls),
    )
  }

  private def get_timers[O, N](
      stmts: Seq[Expr[O]],
      decls: Seq[CFunctionDefinition[O]],
  ): Seq[Timer[O, N]] = {
    Utils.resolve_freertos_constructs[O, N, Timer[O, N]](
      stmts,
      "vesuvTimerCreate",
      (v: Option[CLocal[O]], inv: CInvocation[O]) =>
        Timer.of[O, N](v, inv, decls),
    )
  }

  private def get_isrs[O, N](
      stmts: Seq[Expr[O]],
      decls: Seq[CFunctionDefinition[O]],
  ): Seq[ISR[O, N]] =
    Utils.resolve_freertos_constructs[O, N, ISR[O, N]](
      stmts,
      "vesuvISRCreate",
      (_: Option[CLocal[O]], inv: CInvocation[O]) => ISR.of[O, N](inv, decls),
    )

  private def get_event_groups[O, N](
      stmts: Seq[Expr[O]]
  ): Seq[EventGroup[O, N]] =
    Utils.resolve_freertos_constructs[O, N, EventGroup[O, N]](
      stmts,
      "xEventGroupCreate",
      (v: Option[CLocal[O]], _: CInvocation[O]) => EventGroup[O, N](v),
    )

  private def get_semaphores[O, N](stmts: Seq[Expr[O]]): Seq[Semaphore[O, N]] =
    Utils.resolve_freertos_constructs[O, N, Semaphore[O, N]](
      stmts,
      "xSemaphoreCreateBinary",
      (v: Option[CLocal[O]], _: CInvocation[O]) =>
        BinarySemaphore[O, N](v, is_mutex = false),
    ) ++ Utils.resolve_freertos_constructs[O, N, Semaphore[O, N]](
      stmts,
      "xSemaphoreCreateMutex",
      (v: Option[CLocal[O]], _: CInvocation[O]) =>
        BinarySemaphore[O, N](v, is_mutex = true),
    ) ++ Utils.resolve_freertos_constructs[O, N, Semaphore[O, N]](
      stmts,
      "xSemaphoreCreateRecursiveMutex",
      (v: Option[CLocal[O]], _: CInvocation[O]) => RecursiveMutex[O, N](v),
    )

  private def get_queues[O, N](stmts: Seq[Expr[O]]): Seq[Queue[O, N]] =
    Utils.resolve_freertos_constructs[O, N, Queue[O, N]](
      stmts,
      "xQueueCreate",
      (v: Option[CLocal[O]], inv: CInvocation[O]) => Queue.of[O, N](v, inv),
    )

  private def get_stream_buffers[O, N](
      stmts: Seq[Expr[O]]
  ): Seq[StreamBuffer[O, N]] =
    Utils.resolve_freertos_constructs[O, N, StreamBuffer[O, N]](
      stmts,
      "xStreamBufferCreate",
      (v: Option[CLocal[O]], inv: CInvocation[O]) =>
        StreamBuffer.of[O, N](v, inv),
    )

  private def get_message_buffers[O, N](
      stmts: Seq[Expr[O]]
  ): Seq[MessageBuffer[O, N]] =
    Utils.resolve_freertos_constructs[O, N, MessageBuffer[O, N]](
      stmts,
      "xMessageBufferCreate",
      (v: Option[CLocal[O]], inv: CInvocation[O]) =>
        MessageBuffer.of[O, N](v, inv),
    )
}
