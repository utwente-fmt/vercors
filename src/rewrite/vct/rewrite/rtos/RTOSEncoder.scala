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

  private def get_tasks[O](
      stmts: Seq[Expr[O]],
      decls: Seq[CFunctionDefinition[O]],
  ): Seq[Task[O]] = {
    Utils.resolve_freertos_constructs(
      stmts,
      "vesuvTaskCreate",
      (v: Option[CLocal[O]], inv: CInvocation[O]) => Task.of(v, inv, decls),
    )
  }

  private def get_timers[O](
      stmts: Seq[Expr[O]],
      decls: Seq[CFunctionDefinition[O]],
  ): Seq[Timer[O]] = {
    Utils.resolve_freertos_constructs(
      stmts,
      "vesuvTimerCreate",
      (v: Option[CLocal[O]], inv: CInvocation[O]) => Timer.of(v, inv, decls),
    )
  }

  private def get_isrs[O](
      stmts: Seq[Expr[O]],
      decls: Seq[CFunctionDefinition[O]],
  ): Seq[ISR[O]] =
    Utils.resolve_freertos_constructs(
      stmts,
      "vesuvISRCreate",
      (_: Option[CLocal[O]], inv: CInvocation[O]) => ISR.of(inv, decls),
    )

  private def get_event_groups[O](stmts: Seq[Expr[O]]): Seq[EventGroup[O]] =
    Utils.resolve_freertos_constructs(
      stmts,
      "xEventGroupCreate",
      (v: Option[CLocal[O]], _: CInvocation[O]) => EventGroup(v),
    )

  private def get_semaphores[O](stmts: Seq[Expr[O]]): Seq[Semaphore[O]] =
    Utils.resolve_freertos_constructs(
      stmts,
      "xSemaphoreCreateBinary",
      (v: Option[CLocal[O]], _: CInvocation[O]) =>
        BinarySemaphore(v, is_mutex = false),
    ) ++ Utils.resolve_freertos_constructs(
      stmts,
      "xSemaphoreCreateMutex",
      (v: Option[CLocal[O]], _: CInvocation[O]) =>
        BinarySemaphore(v, is_mutex = true),
    ) ++ Utils.resolve_freertos_constructs(
      stmts,
      "xSemaphoreCreateRecursiveMutex",
      (v: Option[CLocal[O]], _: CInvocation[O]) => RecursiveMutex(v),
    )

  private def get_queues[O](stmts: Seq[Expr[O]]): Seq[Queue[O]] =
    Utils.resolve_freertos_constructs(
      stmts,
      "xQueueCreate",
      (v: Option[CLocal[O]], inv: CInvocation[O]) => Queue.of(v, inv),
    )

  private def get_stream_buffers[O](stmts: Seq[Expr[O]]): Seq[StreamBuffer[O]] =
    Utils.resolve_freertos_constructs(
      stmts,
      "xStreamBufferCreate",
      (v: Option[CLocal[O]], inv: CInvocation[O]) => StreamBuffer.of(v, inv),
    )

  private def get_message_buffers[O](
      stmts: Seq[Expr[O]]
  ): Seq[MessageBuffer[O]] =
    Utils.resolve_freertos_constructs(
      stmts,
      "xMessageBufferCreate",
      (v: Option[CLocal[O]], inv: CInvocation[O]) => MessageBuffer.of(v, inv),
    )
}
