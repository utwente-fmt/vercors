package vct.rewrite.rtos

import vct.col.ast._
import vct.col.util.AstBuildHelpers.{ff, tt}

class StatementTransformer[O, N](col_ir: Transformer[O, N]) {
  def convert(in: Statement[O]): Statement[N] =
    in match {
      case Block(statements) =>
        Block(statements.map(s => convert(s)))(Utils.origen)
      case Branch(branches) =>
        Branch(branches.map(b => (expr_to_expr(b._1), convert(b._2))))(
          Utils.origen
        )
      case Eval(expr) => expr_to_statement(expr)
    }

  private def expr_to_statement(in: Expr[O]): Statement[N] =
    in match {
      case PreAssignExpression(target, value) =>
        Assign(expr_to_expr(target), expr_to_expr(value))(Utils.origen)(
          Utils.origen
        )
      case PostAssignExpression(target, value) =>
        Assign(expr_to_expr(target), expr_to_expr(value))(Utils.origen)(
          Utils.origen
        )
      case CInvocation(applicable, args, givenArgs, yields) =>
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
          case "vPortDisableInterrupts" => ???
          case "vPortEnableInterrupts" => ???
          // Communication API functions
          case "xMessageBufferIsEmpty" | "xMessageBufferIsFull" |
              "xMessageBufferSpacesAvailable" |
              "xMessageBufferReceive" | /*TODO*/ "xMessageBufferReset" |
              "xMessageBufferSend" | "xQueueSendToBack" | "xQueueSendToFront" |
              "xQueueOverwrite" | "xQueueReset" | "xQueueReceive" |
              "xQueuePeek" | "uxQueueSpacesAvailable" |
              "uxQueueMessagesWaiting" | "xQueueIsQueueEmptyFromISR" |
              "xQueueIsQueueFullFromISR" | "uxSemaphoreGetCount" |
              "xSemaphoreGetMutexHolder" | "xSemaphoreGive" |
              "xSemaphoreGiveRecursive" | "xSemaphoreTake" |
              "xSemaphoreTakeRecursive" |
              /*TODO*/ "xStreamBufferBytesAvailable" | "xStreamBufferIsEmpty" |
              "xStreamBufferIsFull" | "xStreamBufferSpacesAvailable" |
              "xStreamBufferReceive" | /*TODO*/ "xStreamBufferReset" |
              "xStreamBufferSend" | "xStreamBufferSetTriggerLevel" =>
            val (f: InstanceField[N], m: InstanceMethod[N]) = col_ir.get_api(
              args.head.asInstanceOf[CLocal[O]],
              Utils.get_applicable_name(applicable),
            )
            InvokeMethod(???, ???, ???, ???, ???, ???, ???)(Utils.origen)(
              Utils.origen
            )
          // Task functions
          case "vTaskDelete" => ???
          case "vTaskDelay" => ???
          case "vTaskDelayUntil" => ???
          case "xTaskAbortDelay" => ???
          case "xTaskGetCurrentTaskHandle" => ???
          case "uxTaskGetNumberOfTasks" => ???
          case "uxTaskPriorityGet" => ???
          case "vTaskPrioritySet" => ???
          case "vTaskResume" => ???
          case "vTaskSuspend" => ???
          case "taskYIELD" => ???
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

  private def expr_to_expr(in: Expr[O]): Expr[N] =
    in match {
      case CLocal(name) =>
        name match {
          case "pdFALSE" | "pdFAIL" => ff
          case "pdTRUE" | "pdPASS" => tt
          case _ => ??? // TODO: Handle variable conversion!
        }
    }
}
