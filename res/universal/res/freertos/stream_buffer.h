#ifndef STREAMBUFFER_H
#define STREAMBUFFER_H

#ifndef FREERTOS_H
    #error Include FreeRTOS.h before including stream_buffer.h!
#endif

#define StreamBufferHandle_t int

#define xStreamBufferCreateStatic( xBufferSizeBytes, xTriggerLevelBytes, pucStreamBufferStorageArea, pxStaticStreamBuffer )             xStreamBufferCreate( xBufferSizeBytes, xTriggerLevelBytes )
StreamBufferHandle_t xStreamBufferCreate( size_t xBufferSizeBytes, size_t xTriggerLevelBytes );
// Ignore, since we do not verify memory
#define vStreamBufferDelete( xStreamBuffer )

size_t xStreamBufferBytesAvailable( StreamBufferHandle_t xStreamBuffer );
BaseType_t xStreamBufferIsEmpty( StreamBufferHandle_t xStreamBuffer );
BaseType_t xStreamBufferIsFull( StreamBufferHandle_t xStreamBuffer );
size_t xStreamBufferSpacesAvailable( StreamBufferHandle_t xStreamBuffer );

#define xStreamBufferReceiveFromISR( xStreamBuffer, pvRxData, xBufferLengthBytes, pxHigherPriorityTaskWoken )           xStreamBufferReceive( xStreamBuffer, pvRxData, xBufferLengthBytes, 0 )
size_t xStreamBufferReceive( StreamBufferHandle_t xStreamBuffer, void *pvRxData, size_t xBufferLengthBytes, TickType_t xTicksToWait );

BaseType_t xStreamBufferReset( StreamBufferHandle_t xStreamBuffer );

#define xStreamBufferSendFromISR( xStreamBuffer, pvTxData, xDataLengthBytes, pxHigherPriorityTaskWoken )            xStreamBufferSend( xStreamBuffer, pvTxData, xDataLengthBytes, 0 )
size_t xStreamBufferSend( StreamBufferHandle_t xStreamBuffer, const void *pvTxData, size_t xDataLengthBytes, TickType_t xTicksToWait );

BaseType_t xStreamBufferSetTriggerLevel( StreamBufferHandle_t xStreamBuffer, size_t xTriggerLevel );

#endif