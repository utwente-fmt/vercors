package vct.rewrite.rtos

import vct.col.ast._
import vct.col.rewrite.{Generation, Rewritten}
import vct.rewrite.rtos.freertosir._

object RTOSEncoder {
  def transform[O <: Generation](
      c_model: Seq[GlobalDeclaration[O]]
  ): Seq[GlobalDeclaration[Rewritten[O]]] = {
    val global_vars: Seq[(CInit[O], Type[Rewritten[O]])] = get_global_vars(
      c_model.head
    )
    val def_methods: Seq[CFunctionDefinition[O]] = c_model.head.collect {
      case f: CFunctionDefinition[O] => f
    }
    val main: CFunctionDefinition[O] = get_main_method(def_methods)
    val stmts: Seq[Expr[O]] = main.body.collect { case Eval(expr) => expr }

    new COLEncoder[O](
      global_vars,
      def_methods,
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

  private def get_global_vars[O <: Generation](
      decl: GlobalDeclaration[O]
  ): Seq[(CInit[O], Type[Rewritten[O]])] =
    decl.collect { case d: CGlobalDeclaration[O] => d }
      .flatMap(d => d.decl.inits.map(i => (i, Utils.get_ctype(d.decl.specs))))
      .filter(d =>
        d._1.decl match {
          case CName(_) => true
          case _ => false
        }
      )

  private def get_main_method[O](
      decls: Seq[CFunctionDefinition[O]]
  ): CFunctionDefinition[O] =
    decls.find(f => Utils.get_declarator_name(f.declarator).equals("main"))
      .getOrElse(throw new IllegalStateException("No main method declared!"))

  private def get_tasks[O <: Generation](
      stmts: Seq[Expr[O]],
      decls: Seq[CFunctionDefinition[O]],
  ): Seq[Task[O]] =
    Utils.resolve_freertos_constructs[O, Task[O]](
      stmts,
      "vesuvTaskCreate",
      (v: Option[CLocal[O]], inv: CInvocation[O]) => Task.of(v, inv, decls),
    )

  private def get_timers[O <: Generation](
      stmts: Seq[Expr[O]],
      decls: Seq[CFunctionDefinition[O]],
  ): Seq[Timer[O]] = {
    val timers = Utils.resolve_freertos_constructs[O, Timer[O]](
      stmts,
      "vesuvTimerCreate",
      (v: Option[CLocal[O]], inv: CInvocation[O]) => Timer.of(v, inv, decls),
    )
    activate(stmts, timers)
    timers
  }

  private def get_isrs[O <: Generation](
      stmts: Seq[Expr[O]],
      decls: Seq[CFunctionDefinition[O]],
  ): Seq[ISR[O]] =
    Utils.resolve_freertos_constructs[O, ISR[O]](
      stmts,
      "vesuvISRCreate",
      (_: Option[CLocal[O]], inv: CInvocation[O]) => ISR.of(inv, decls),
    )

  private def get_event_groups[O <: Generation](
      stmts: Seq[Expr[O]]
  ): Seq[EventGroup[O]] =
    Utils.resolve_freertos_constructs[O, EventGroup[O]](
      stmts,
      "xEventGroupCreate",
      (v: Option[CLocal[O]], _: CInvocation[O]) => EventGroup(v),
    )

  private def get_semaphores[O <: Generation](
      stmts: Seq[Expr[O]]
  ): Seq[Semaphore[O]] =
    Utils.resolve_freertos_constructs[O, Semaphore[O]](
      stmts,
      "xSemaphoreCreateBinary",
      (v: Option[CLocal[O]], _: CInvocation[O]) =>
        BinarySemaphore(v, is_mutex = false),
    ) ++ Utils.resolve_freertos_constructs[O, Semaphore[O]](
      stmts,
      "xSemaphoreCreateMutex",
      (v: Option[CLocal[O]], _: CInvocation[O]) =>
        BinarySemaphore(v, is_mutex = true),
    ) ++ Utils.resolve_freertos_constructs[O, Semaphore[O]](
      stmts,
      "xSemaphoreCreateRecursiveMutex",
      (v: Option[CLocal[O]], _: CInvocation[O]) => RecursiveMutex(v),
    )

  private def get_queues[O <: Generation](stmts: Seq[Expr[O]]): Seq[Queue[O]] =
    Utils.resolve_freertos_constructs[O, Queue[O]](
      stmts,
      "xQueueCreate",
      (v: Option[CLocal[O]], inv: CInvocation[O]) => Queue.of(v, inv),
    )

  private def get_stream_buffers[O <: Generation](
      stmts: Seq[Expr[O]]
  ): Seq[StreamBuffer[O]] =
    Utils.resolve_freertos_constructs[O, StreamBuffer[O]](
      stmts,
      "xStreamBufferCreate",
      (v: Option[CLocal[O]], inv: CInvocation[O]) => StreamBuffer.of(v, inv),
    )

  private def get_message_buffers[O <: Generation](
      stmts: Seq[Expr[O]]
  ): Seq[MessageBuffer[O]] =
    Utils.resolve_freertos_constructs[O, MessageBuffer[O]](
      stmts,
      "xMessageBufferCreate",
      (v: Option[CLocal[O]], inv: CInvocation[O]) => MessageBuffer.of(v, inv),
    )

  private def activate[O <: Generation](
      stmts: Seq[Expr[O]],
      timers: Seq[Timer[O]],
  ): Unit = {
    val timer_activations: Seq[CInvocation[O]] = stmts.collect {
      case PreAssignExpression(_, value) if (value match {
            case CInvocation(applicable, _, _, _) =>
              Utils.get_applicable_name(applicable).equals("xTimerStart") ||
              Utils.get_applicable_name(applicable).equals("xTimerReset")
            case _ => false
          }) =>
        value.asInstanceOf[CInvocation[O]]
      case inv: CInvocation[O]
          if Utils.get_applicable_name(inv.applicable).equals("xTimerStart") ||
            Utils.get_applicable_name(inv.applicable).equals("xTimerReset") =>
        inv
    }
    val activated_timers: Seq[String] = timer_activations
      .map(inv => inv.args.head.asInstanceOf[CLocal[O]].name)
    timers.foreach(t =>
      if (t.decl.nonEmpty && activated_timers.contains(t.decl.get.name))
        t.activate()
    )
  }
}
