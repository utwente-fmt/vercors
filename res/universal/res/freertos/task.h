#ifndef TASK_H
#define TASK_H

#ifndef FREERTOS_H
    #error Include FreeRTOS.h before including task.h!
#endif

#define eTaskState int
#define eRunning 0
#define eReady 1
#define eBlocked 2
#define eSuspended 3
#define eDeleted 4

#define eNotifyAction int
#define eNoAction 0
#define eSetBits 1
#define eIncrement 2
#define eSetValueWithOverwrite 3
#define eSetValueWithoutOverwrite 4

// Disallow advanced low-level features from the Task API that would interfere with verification
//
// void portSWITCH_TO_USER_MODE( void );
// void vTaskAllocateMPURegions( TaskHandle_t xTaskToModify, const MemoryRegion_t * const xRegions );
// BaseType_t xTaskCallApplicationTaskHook( TaskHandle_t xTask, void *pvParameters );
// TaskHookFunction_t xTaskGetApplicationTaskTag( TaskHandle_t xTask );
// void vTaskSetApplicationTaskTag( TaskHandle_t xTask, TaskHookFunction_t pxTagValue );
// BaseType_t xTaskCatchUpTicks( TickType_t xTicksToCatchUp );
// BaseType_t xTaskCheckForTimeOut( TimeOut_t * const pxTimeOut, TickType_t * const pxTicksToWait );
// BaseType_t xTaskCreateRestricted( TaskParameters_t *pxTaskDefinition, TaskHandle_t *pxCreatedTask );
// TaskHandle_t xTaskGetIdleTaskHandle( void );
// TaskHandle_t xTaskGetHandle( const char *pcNameToQuery );
// void vTaskGetRunTimeStats( char *pcWriteBuffer );
// BaseType_t xTaskGetSchedulerState( void );
// UBaseType_t uxTaskGetStackHighWaterMark( TaskHandle_t xTask );
// UBaseType_t uxTaskGetSystemState( TaskStatus_t * const pxTaskStatusArray, const UBaseType_t uxArraySize, unsigned long * const pulTotalRunTime );
// void vTaskGetTaskInfo( TaskHandle_t xTask, TaskStatus_t *pxTaskStatus, BaseType_t xGetFreeStackSpace, eTaskState eState );
// void *pvTaskGetThreadLocalStoragePointer( TaskHandle_t xTaskToQuery, BaseType_t xIndex );
// void vTaskSetThreadLocalStoragePointer( TaskHandle_t xTaskToSet, BaseType_t xIndex, void *pvValue );
// void vTaskList( char *pcWriteBuffer );
// BaseType_t xTaskResumeAll( void );
// void vTaskSuspendAll( void );
// void vTaskSetTimeOutState( TimeOut_t * const pxTimeOut );
// void vTaskStepTick( TickType_t xTicksToJump );

#define tskIDLE_PRIORITY 0

#define xTaskCreate( pvTaskCode, pcName, usStackDepth, pvParameters, uxPriority, pxCreatedTask )                            vesuvTaskCreate( pvTaskCode( pvParameters ), uxPriority )
#define xTaskCreateStatic( pvTaskCode, pcName, ulStackDepth, pvParameters, uxPriority, puxStackBuffer, pxTaskBuffer )       vesuvTaskCreate( pvTaskCode( pvParameters ), uxPriority )
BaseType_t vesuvTaskCreate( void vesuvIGNORE, UBaseType_t uxPriority );
void vTaskDelete( TaskHandle_t pxTask );

#define xTaskGetTickCount()           0
#define xTaskGetTickCountFromISR()    0

void vTaskDelay( TickType_t xTicksToDelay );
void vTaskDelayUntil( TickType_t *pxPreviousWakeTime, TickType_t xTimeIncrement );

BaseType_t xTaskAbortDelay( TaskHandle_t xTask );

TaskHandle_t xTaskGetCurrentTaskHandle();
UBaseType_t uxTaskGetNumberOfTasks();
eTaskState eTaskGetState( TaskHandle_t pxTask );
// Ignore task names, they are not relevant for verification
#define pcTaskGetName( xTaskToQuery )       ""

#define xTaskNotifyFromISR( xTaskToNotify, ulValue, eAction, pxHigherPriorityTaskWoken )         xTaskNotify( xTaskToNotify, ulValue, eAction )
BaseType_t xTaskNotify( TaskHandle_t xTaskToNotify, uint32_t ulValue, eNotifyAction eAction );
#define xTaskNotifyAndQueryFromISR( xTaskToNotify, ulValue, eAction, pulPreviousNotifyValue, pxHigherPriorityTaskWoken )       taskNotifyAndQuery( xTaskToNotify, ulValue, eAction, pulPreviousNotifyValue )
BaseType_t xTaskNotifyAndQuery( TaskHandle_t xTaskToNotify, uint32_t ulValue, eNotifyAction eAction, uint32_t *pulPreviousNotifyValue );
#define vTaskNotifyGiveFromISR( xTaskToNotify, pxHigherPriorityTaskWoken )              xTaskNotifyGive( xTaskToNotify )
BaseType_t xTaskNotifyGive( TaskHandle_t xTaskToNotify );
BaseType_t xTaskNotifyStateClear( TaskHandle_t xTask );
uint32_t ulTaskNotifyTake( BaseType_t xClearCountOnExit, TickType_t xTicksToWait );
BaseType_t xTaskNotifyWait( uint32_t ulBitsToClearOnEntry, uint32_t ulBitsToClearOnExit, uint32_t *pulNotificationValue, TickType_t xTicksToWait );

UBaseType_t uxTaskPriorityGet( TaskHandle_t pxTask );
void vTaskPrioritySet( TaskHandle_t pxTask, UBaseType_t uxNewPriority );

#define xTaskResumeFromISR( pxTaskToResume )            vTaskResume( pxTaskToResume )
BaseType_t vTaskResume( TaskHandle_t pxTaskToResume );
void vTaskSuspend( TaskHandle_t pxTaskToSuspend );

void vTaskStartScheduler();

void taskYIELD();

#endif