#ifndef TIMERS_H
#define TIMERS_H

#ifndef FREERTOS_H
    #error Include FreeRTOS.h before including timers.h!
#endif

#define TimerHandle_t int

typedef void (* TimerCallbackFunction_t)( TimerHandle_t xTimer );

// Disallow unsupported functions
//
// BaseType_t xTimerChangePeriod( TimerHandle_t xTimer, TickType_t xNewPeriod, TickType_t xTicksToWait );
// BaseType_t xTimerChangePeriodFromISR( TimerHandle_t xTimer, TickType_t xNewPeriod, BaseType_t *pxHigherPriorityTaskWoken );
// TickType_t xTimerGetExpiryTime( TimerHandle_t xTimer );
// TaskHandle_t xTimerGetTimerDaemonTaskHandle( void );
// void *pvTimerGetTimerID( TimerHandle_t xTimer );
// void vTimerSetTimerID( TimerHandle_t xTimer, void *pvNewID );
// BaseType_t xTimerPendFunctionCall( PendedFunction_t xFunctionToPend, void *pvParameter1, uint32_t ulParameter2, TickType_t xTicksToWait );
// BaseType_t xTimerPendFunctionCallFromISR( PendedFunction_t xFunctionToPend, void *pvParameter1, uint32_t ulParameter2, BaseType_t *pxHigherPriorityTaskWoken );

#define xTimerCreateStatic( pcTimerName, xTimerPeriod, uxAutoReload, pvTimerID, pxCallbackFunction, pxTimerBuffer )     vesuvTimerCreate( xTimerPeriod, uxAutoReload, configTIMER_TASK_PRIORITY, pxCallbackFunction() )
#define xTimerCreate( pcTimerName, xTimerPeriod, uxAutoReload, pvTimerID, pxCallbackFunction )                          vesuvTimerCreate( xTimerPeriod, uxAutoReload, configTIMER_TASK_PRIORITY, pxCallbackFunction() )
TimerHandle_t vesuvTimerCreate( const TickType_t xTimerPeriod, const UBaseType_t uxAutoReload, const UBaseType_t vesuvPriority, void *vesuvIGNORE );
// Ignore, since we do not verify memory
#define xTimerDelete( xTimer, xTicksToWait )

// Ignore timer name for verification
#define pcTimerGetName( TimerHandle_t xTimer )        ""

TickType_t xTimerGetPeriod( TimerHandle_t xTimer );
TickType_t uxTimerGetReloadMode( TimerHandle_t xTimer );
BaseType_t xTimerIsTimerActive( TimerHandle_t xTimer );

// TODO: We abstract from the timer queue in the Daemon task, thus these never wait
#define xTimerResetFromISR( xTimer, pxHigherPriorityTaskWoken )             xTimerReset( xTimer, 0 )
BaseType_t xTimerReset( TimerHandle_t xTimer, TickType_t xTicksToWait );
#define xTimerStartFromISR( xTimer, pxHigherPriorityTaskWoken )             xTimerStart( xTimer, 0 )
BaseType_t xTimerStart( TimerHandle_t xTimer, TickType_t xTicksToWait );
#define xTimerStopFromISR( xTimer, pxHigherPriorityTaskWoken )              xTimerStop( xTimer, 0 )
BaseType_t xTimerStop( TimerHandle_t xTimer, TickType_t xTicksToWait );


#endif