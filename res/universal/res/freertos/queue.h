#ifndef QUEUE_H
#define QUEUE_H

#ifndef FREERTOS_H
    #error Include FreeRTOS.h before including queue.h!
#endif

#define QueueHandle_t int

#define xQueueCreateStatic( uxQueueLength, uxItemSize, pucQueueStorageBuffer, pxQueueBuffer )           xQueueCreate( uxQueueLength, uxItemSize )
QueueHandle_t xQueueCreate( UBaseType_t uxQueueLength, UBaseType_t uxItemSize );
// Ignore, since we do not verify memory
#define vQueueDelete( pxQueueToDelete )

#define xQueueSendFromISR( xQueue, pvItemToQueue, pxHigherPriorityTaskWoken )             xQueueSend( xQueue, pvItemToQueue, 0 )
#define xQueueSendToBackFromISR( xQueue, pvItemToQueue, pxHigherPriorityTaskWoken )       xQueueSendToBack( xQueue, pvItemToQueue, 0 )
#define xQueueSendToFrontFromISR( xQueue, pvItemToQueue, pxHigherPriorityTaskWoken )      xQueueSendToFront( xQueue, pvItemToQueue, 0 )
#define xQueueSend( xQueue, pvItemToQueue, xTicksToWait )       xQueueSendToBack( (xQueue), (pvItemToQueue), (xTicksToWait) )
BaseType_t xQueueSendToBack( QueueHandle_t xQueue, const void *pvItemToQueue, TickType_t xTicksToWait );
BaseType_t xQueueSendToFront( QueueHandle_t xQueue, const void *pvItemToQueue, TickType_t xTicksToWait );

#define xQueueOverwriteFromISR( xQueue, pvItemToQueue )         xQueueOverwrite( xQueue, pvItemToQueue )
BaseType_t xQueueOverwrite( QueueHandle_t xQueue, const void *pvItemToQueue );

BaseType_t xQueueReset( QueueHandle_t xQueue );

#define xQueueReceiveFromISR( xQueue, pvBuffer, pxHigherPriorityTaskWoken )     xQueueReceive( xQueue, pvBuffer, 0 )
BaseType_t xQueueReceive( QueueHandle_t xQueue, void *pvBuffer, TickType_t xTicksToWait );

#define xQueuePeekFromISR( xQueue, pvBuffer )           xQueuePeek( xQueue, pvBuffer, 0 )
BaseType_t xQueuePeek( QueueHandle_t xQueue, void *pvBuffer, TickType_t xTicksToWait );

UBaseType_t uxQueueSpacesAvailable( const QueueHandle_t xQueue );
UBaseType_t uxQueueMessagesWaiting( const QueueHandle_t xQueue );
BaseType_t xQueueIsQueueEmptyFromISR( const QueueHandle_t pxQueue );
BaseType_t xQueueIsQueueFullFromISR( const QueueHandle_t pxQueue );

#endif