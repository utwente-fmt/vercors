package vct.rewrite.rtos

import vct.col.ast._
import vct.col.ref.{DirectRef, LazyRef}
import vct.col.util.AstBuildHelpers.{ff, tt}

class StatementTransformer[O, N](
    col_ir: Transformer[O, N],
    tid: Option[Int],
    scheduler: Option[InstanceField[N]],
    this_in_scheduler: InstanceField[N],
) {
  def convert(in: Statement[O]): Statement[N] =
    in match {
      case Block(statements) =>
        Block(statements.map(s => convert(s)))(Utils.origen)
      case Branch(branches) =>
        // TODO: What about side effects in branch conditions?
        Branch(branches.map(b => (expr_to_expr(b._1)._2, convert(b._2))))(
          Utils.origen
        )
      case Eval(expr) => expr_to_statement(expr)
    }

  private def expr_to_statement(in: Expr[O]): Statement[N] =
    in match {
      case PreAssignExpression(target, value) =>
        val (v_pre: Seq[Statement[N]], v_expr: Expr[N]) = expr_to_expr(value)
        val assign: Statement[N] =
          Assign(expr_to_expr(target)._2, v_expr)(Utils.blame)(Utils.origen)
        if (v_pre.isEmpty)
          assign
        else
          Block(v_pre :+ assign)(Utils.origen)
      case PostAssignExpression(target, value) =>
        val (v_pre: Seq[Statement[N]], v_expr: Expr[N]) = expr_to_expr(value)
        val assign: Statement[N] =
          Assign(expr_to_expr(target)._2, v_expr)(Utils.blame)(Utils.origen)
        if (v_pre.isEmpty)
          assign
        else
          Block(v_pre :+ assign)(Utils.origen)
      case CInvocation(applicable, args, _, _) =>
        Utils.get_applicable_name(applicable) match {
          // Functions that are allowed only in the main() function
          case "vTaskStartScheduler" | "xEventGroupCreate" | "vesuvISRCreate" |
              "xMessageBufferCreate" | "xQueueCreate" |
              "xSemaphoreCreateBinary" | "xSemaphoreCreateMutex" |
              "xSemaphoreCreateRecursiveMutex" | "xStreamBufferCreate" |
              "vesuvTaskCreate" | "vesuvTimerCreate" =>
            throw new IllegalArgumentException(
              "Function " + Utils.get_applicable_name(applicable) +
                " must not be called outside main()!"
            )
          // Event groups
          case "xEventGroupClearBits" => ???
          case "xEventGroupGetBits" => ???
          case "xEventGroupSetBits" => ???
          case "xEventGroupSync" => ???
          case "xEventGroupWaitBits" => ???
          // Interrupt management
          case "vPortDisableInterrupts" =>
            Block(col_ir.get_isr_locks.map(f =>
              Lock(Utils.deref_of(
                f,
                Some(Utils.deref_of(Utils.exclude_isr(scheduler))),
              ))(Utils.blame)(Utils.origen)
            ))(Utils.origen)
          case "vPortEnableInterrupts" =>
            Block(col_ir.get_isr_locks.map(f =>
              Unlock(Utils.deref_of(
                f,
                Some(Utils.deref_of(Utils.exclude_isr(scheduler))),
              ))(Utils.blame)(Utils.origen)
            ))(Utils.origen)
          // Communication API functions - no parameters except API construct
          case "xMessageBufferIsEmpty" | "xMessageBufferIsFull" |
              "xMessageBufferSpacesAvailable" |
              /*TODO*/ "xMessageBufferReset" | "xQueueReset" |
              "uxQueueSpacesAvailable" | "uxQueueMessagesWaiting" |
              "xQueueIsQueueEmptyFromISR" | "xQueueIsQueueFullFromISR" |
              "uxSemaphoreGetCount" | "xSemaphoreGetMutexHolder" |
              "xSemaphoreGive" | "xSemaphoreGiveRecursive" |
              /*TODO*/ "xStreamBufferBytesAvailable" | "xStreamBufferIsEmpty" |
              "xStreamBufferIsFull" | "xStreamBufferSpacesAvailable" |
              /*TODO*/ "xStreamBufferReset" | /*TODO*/ "xStreamBufferSetTriggerLevel" =>
            resolve_api_call(
              args.head.asInstanceOf[CLocal[O]],
              Utils.get_applicable_name(applicable),
              None,
              None,
              None,
              None,
            )
          case "xQueueSendToBack" | "xQueueSendToFront" =>
            resolve_api_call(
              args.head.asInstanceOf[CLocal[O]],
              Utils.get_applicable_name(applicable),
              Some(args(1)),
              Some(args(2)),
              Some(col_ir.get_read_event),
              None,
            )
          case "xQueueOverwrite" =>
            resolve_api_call(
              args.head.asInstanceOf[CLocal[O]],
              Utils.get_applicable_name(applicable),
              Some(args(1)),
              None,
              None,
              None,
            )
          case "xQueueReceive" | "xQueuePeek" =>
            resolve_api_call(
              args.head.asInstanceOf[CLocal[O]],
              Utils.get_applicable_name(applicable),
              None,
              Some(args(2)),
              Some(col_ir.get_write_event),
              Some(args(1)),
            )
          case "xSemaphoreTakeRecursive" | "xSemaphoreTake" =>
            resolve_api_call(
              args.head.asInstanceOf[CLocal[O]],
              Utils.get_applicable_name(applicable),
              None,
              Some(args(1)),
              Some(col_ir.get_write_event),
              None,
            )
          // TODO: Support for these needs pointer resolution in the variable handling!
          case "xMessageBufferSend" | "xStreamBufferSend" =>
            resolve_api_call(
              args.head.asInstanceOf[CLocal[O]],
              Utils.get_applicable_name(applicable),
              Some(args(1)),
              Some(args(3)),
              Some(col_ir.get_read_event),
              None,
            )
          case "xMessageBufferReceive" | "xStreamBufferReceive" =>
            resolve_api_call(
              args.head.asInstanceOf[CLocal[O]],
              Utils.get_applicable_name(applicable),
              None,
              Some(args(3)),
              Some(col_ir.get_read_event),
              Some(args(1)),
            )
          // Task functions
          case "vTaskDelete" =>
            // Make task wait for event that will never be scheduled
            Utils.update_scheduling_variable(
              col_ir.get_taskState,
              Utils.exclude_isr(scheduler),
              Utils.int_val(col_ir.get_tid(args.head.asInstanceOf[CLocal[O]])),
              Utils.int_val(col_ir.reserve_event_id),
            )
          case "vTaskDelay" =>
            wait_loop(
              col_ir.reserve_event_id,
              Some(
                Utils.int_val(Utils.resolve_integer(args.head, "task delay"))
              ),
            )
          case "vTaskDelayUntil" =>
            wait_loop(
              col_ir.reserve_event_id,
              Some(
                Minus(
                  Utils.int_val(Utils.resolve_integer(args.head, "task delay")),
                  SeqSubscript(
                    Utils.deref_ref(
                      new LazyRef[N, InstanceField[N]](col_ir.get_taskWaitTime),
                      Utils.deref_of(Utils.exclude_isr(scheduler)),
                    ),
                    Utils.int_val(tid.get),
                  )(Utils.blame)(Utils.origen),
                )(Utils.origen)
              ),
            )
          case "xTaskAbortDelay" =>
            Utils.update_scheduling_variable(
              col_ir.get_taskState,
              Utils.exclude_isr(scheduler),
              Utils.int_val(col_ir.get_tid(args.head.asInstanceOf[CLocal[O]])),
              Utils.int_val(-1),
            )
          case "xTaskGetCurrentTaskHandle" =>
            Eval(Utils.int_val(tid.get))(Utils.origen)
          case "uxTaskGetNumberOfTasks" =>
            Eval(Utils.int_val(col_ir.get_n_tasks))(Utils.origen)
          case "uxTaskPriorityGet" =>
            Eval(Utils.scheduling_variable_entry(
              col_ir.get_taskPriority,
              Utils.exclude_isr(scheduler),
              Utils.int_val(col_ir.get_tid(args.head.asInstanceOf[CLocal[O]])),
            ))(Utils.origen)
          case "vTaskPrioritySet" =>
            Utils.update_scheduling_variable(
              col_ir.get_taskPriority,
              Utils.exclude_isr(scheduler),
              Utils.int_val(col_ir.get_tid(args.head.asInstanceOf[CLocal[O]])),
              Utils.int_val(Utils.resolve_integer(args(1), "task priority")),
            )
          case "vTaskResume" => ???
          case "vTaskSuspend" => ???
          case "taskYIELD" => wait_loop(col_ir.reserve_event_id, None)
          // Task notification
          case "xTaskNotify" => ???
          case "xTaskNotifyAndQuery" => ???
          case "xTaskNotifyGive" => ???
          case "xTaskNotifyStateClear" => ???
          case "ulTaskNotifyTake" => ???
          case "xTaskNotifyWait" => ???
          // Timer functions
          case "xTimerGetPeriod" => ???
          case "uxTimerGetReloadMode" => ???
          case "xTimerIsTimerActive" => ???
          case "xTimerReset" => ???
          case "xTimerStart" => ???
          case "xTimerStop" => ???
          case _ =>
            ??? // TODO: Handle regular method call and add it to task/timer/ISR class!
        }
    }

  private def expr_to_expr(in: Expr[O]): (Seq[Statement[N]], Expr[N]) =
    in match {
      case CLocal(name) =>
        name match {
          case "pdFALSE" | "pdFAIL" => (Seq(), ff)
          case "pdTRUE" | "pdPASS" => (Seq(), tt)
          case _ => ??? // TODO: Handle variable conversion!
        }
    }

  private def resolve_api_call(
      cvar: CLocal[O],
      func_name: String,
      arg: Option[Expr[O]],
      delay: Option[Expr[O]],
      get_eid: Option[InstanceField[N] => Int],
      store: Option[Expr[O]],
  ): Statement[N] = {
    val (f: InstanceField[N], m: InstanceMethod[N]) = col_ir
      .get_api(cvar, func_name)
    val field: Deref[N] = Utils
      .deref_of(f, Some(Utils.deref_of(Utils.exclude_isr(scheduler))))
    val (pre_stmts: Seq[Statement[N]], args: Seq[Expr[N]]) =
      arg match {
        case Some(expr) =>
          expr_to_expr(expr) match { case (s, e) => (s, Seq(e)) }
        case None => (Seq(), Seq())
      }
    val method_call: InvokeMethod[N] =
      InvokeMethod(
        field,
        new DirectRef[N, InstanceMethod[N]](m),
        args,
        Seq(),
        Seq(),
        Seq(),
        Seq(),
      )(Utils.blame)(Utils.origen)

    val call_stmts: Seq[Statement[N]] =
      delay match {
        case Some(d) =>
          create_delay_if_necessary(
            d,
            get_eid.getOrElse(
              throw new IllegalArgumentException(
                "Delay specified but event not found"
              )
            )(f),
            col_ir.get_call_condition(m)(args),
            method_call,
          )
        case None => Seq(method_call)
      }

    val (pre_store: Seq[Statement[N]], store_stmts: Seq[Statement[N]]) =
      store match {
        case Some(st) =>
          expr_to_expr(st) match {
            case (s, e) =>
              (
                s,
                Seq(
                  Assign(
                    e,
                    Utils.deref_of(col_ir.get_output_field(f), Some(field)),
                  )(Utils.blame)(Utils.origen)
                ),
              )
          }
        case None => (Seq(), Seq())
      }

    Block(pre_stmts ++ pre_store ++ call_stmts ++ store_stmts)(Utils.origen)
  }

  private def create_delay_if_necessary(
      delay_expr: Expr[O],
      eid: Int,
      cond: Expr[N],
      call: Statement[N],
  ): Seq[Statement[N]] =
    delay_expr match {
      case CLocal(name) if name.equals("portMAX_DELAY") =>
        Seq(
          Loop(
            Utils.skip,
            cond,
            Utils.skip,
            Utils.to_loop_invariant(
              get_default_contract(holding_global_lock = true, runnable = true)
            ),
            wait_loop(eid, None),
          )(Utils.origen),
          call,
        )
      case _ =>
        val delay: Int = Utils.resolve_integer(delay_expr, "API call wait time")
        if (delay == 0)
          Seq(call)
        else
          Seq(
            Loop(
              Utils.skip,
              cond,
              Utils.skip,
              Utils.to_loop_invariant(get_default_contract(
                holding_global_lock = true,
                runnable = true,
              )),
              wait_loop(eid, Some(Utils.int_val(delay))),
            )(Utils.origen),
            call,
          )
    }

  def wait_loop(eid: Int, timeout: Option[Expr[N]]): Statement[N] =
    Utils.task_wait(
      col_ir,
      Utils.exclude_isr(scheduler),
      get_default_contract(holding_global_lock = true, runnable = false),
      tid.get,
      eid,
      timeout,
    )

  def get_default_contract(
      holding_global_lock: Boolean,
      runnable: Boolean,
  ): Expr[N] = {
    val s: InstanceField[N] = Utils.exclude_isr(scheduler)
    var conds: Seq[Expr[N]] = Seq(
      Perm(Utils.loc_of(s), Utils.read)(Utils.origen),
      Neq(Utils.deref_of(s), Utils.nul)(Utils.origen),
      Committed(Utils.deref_of(s))(Utils.blame)(Utils.origen),
    )
    if (holding_global_lock) {
      conds ++= Seq(
        Held(Utils.deref_of(s))(Utils.origen),
        Utils.predicate_apply(
          Utils.deref_of(s),
          new LazyRef(col_ir.get_globalInvariant),
          Seq(),
        ),
      )
    } else {
      conds ++= Seq(
        Perm(
          Utils.loc_of(this_in_scheduler, Some(Utils.deref_of(s))),
          Utils.read,
        )(Utils.origen)
      )
    }
    conds ++= Seq(
      Eq(
        Utils.thiz,
        Utils.deref_of(this_in_scheduler, Some(Utils.deref_of(s))),
      )(Utils.origen)
    )
    if (runnable) {
      val taskState_deref: Deref[N] = Utils
        .deref_ref(new LazyRef(col_ir.get_taskState), Utils.deref_of(s))
      conds ++= Seq(
        Eq(
          SeqSubscript(taskState_deref, Utils.int_val(tid.get))(Utils.blame)(
            Utils.origen
          ),
          Utils.int_val(-2),
        )(Utils.origen)
      )
    }
    Utils.fold_star(conds)
  }
}
