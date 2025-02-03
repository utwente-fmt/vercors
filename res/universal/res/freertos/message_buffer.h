#ifndef MESSAGEBUFFER_H
#define MESSAGEBUFFER_H

#ifndef FREERTOS_H
    #error Include FreeRTOS.h before including event_groups.h!
#endif

#define MessageBufferHandle_t int

#define xMessageBufferCreateStatic( xBufferSizeBytes, pucMessageBufferStorageArea, pxStaticMessageBuffer )              xMessageBufferCreate( xBufferSizeBytes )
MessageBufferHandle_t xMessageBufferCreate( size_t xBufferSizeBytes );
// Ignore, since we do not verify memory
#define vMessageBufferDelete( xMessageBuffer )

BaseType_t xMessageBufferIsEmpty( MessageBufferHandle_t xMessageBuffer );
BaseType_t xMessageBufferIsFull( MessageBufferHandle_t xMessageBuffer );
size_t xMessageBufferSpacesAvailable( MessageBufferHandle_t xMessageBuffer );

#define xMessageBufferReceiveFromISR( xMessageBuffer, pvRxData, xBufferLengthBytes, pxHigherPriorityTaskWoken )         xMessageBufferReceive( xMessageBuffer, pvRxData, xBufferLengthBytes, 0 )
size_t xMessageBufferReceive( MessageBufferHandle_t xMessageBuffer, void *pvRxData, size_t xBufferLengthBytes, TickType_t xTicksToWait );

BaseType_t xMessageBufferReset( MessageBufferHandle_t xMessageBuffer );

#define xMessageBufferSendFromISR( xMessageBuffer, pvTxData, xDataLengthBytes, pxHigherPriorityTaskWoken )              xMessageBufferSend( xMessageBuffer, pvTxData, xDataLengthBytes, 0 )
size_t xMessageBufferSend( MessageBufferHandle_t xMessageBuffer, const void *pvTxData, size_t xDataLengthBytes, TickType_t xTicksToWait );

#endif