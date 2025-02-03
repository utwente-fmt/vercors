#ifndef SEMPHR_H
#define SEMPHR_H

#ifndef FREERTOS_H
    #error Include FreeRTOS.h before including semphr.h!
#endif

#define SemaphoreHandle_t int

void vSemaphoreCreateBinary( SemaphoreHandle_t xSemaphore );
#define xSemaphoreCreateBinaryStatic( pxSemaphoreBuffer )       xSemaphoreCreateBinary()
SemaphoreHandle_t xSemaphoreCreateBinary();
#define xSemaphoreCreateCountingStatic( uxMaxCount, uxInitialCount, pxSempahoreBuffer )             xSemaphoreCreateCounting( uxMaxCount, uxInitialCount )
SemaphoreHandle_t xSemaphoreCreateCounting( UBaseType_t uxMaxCount, UBaseType_t uxInitialCount );
#define xSemaphoreCreateMutexStatic( pxMutexBuffer )            xSemaphoreCreateMutex()
SemaphoreHandle_t xSemaphoreCreateMutex();
#define xSemaphoreCreateRecursiveMutexStatic( pxMutexBuffer )           xSemaphoreCreateRecursiveMutex()
SemaphoreHandle_t xSemaphoreCreateRecursiveMutex();
// Ignore, since we do not verify memory
#define vSemaphoreDelete( xSemaphore )

UBaseType_t uxSemaphoreGetCount( SemaphoreHandle_t xSemaphore );
TaskHandle_t xSemaphoreGetMutexHolder( SemaphoreHandle_t xMutex );

#define xSemaphoreGiveFromISR( xSemaphore, pxHigherPriorityTaskWoken )          xSemaphoreGive( xSemaphore )
BaseType_t xSemaphoreGive( SemaphoreHandle_t xSemaphore );
BaseType_t xSemaphoreGiveRecursive( SemaphoreHandle_t xMutex );

#define xSemaphoreTakeFromISR( xSemaphore, pxHigherPriorityTaskWoken )          xSemaphoreTake( xSemaphore, 0 )
BaseType_t xSemaphoreTake( SemaphoreHandle_t xSemaphore, TickType_t xTicksToWait );
BaseType_t xSemaphoreTakeRecursive( SemaphoreHandle_t xMutex, TickType_t xTicksToWait );

#endif