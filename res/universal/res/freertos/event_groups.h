#ifndef EVENTGROUPS_H
#define EVENTGROUPS_H

#ifndef FREERTOS_H
    #error Include FreeRTOS.h before including event_groups.h!
#endif

#define EventGroupHandle_t int
#define EventBits_t int

#define xEventGroupCreateStatic( pxEventGroupBuffer )           xEventGroupCreate()
EventGroupHandle_t xEventGroupCreate();
// Deletion can be ignored, since memory is not verified
#define vEventGroupDelete( xEventGroup )

#define xEventGroupClearBitsFromISR( xEventGroup, uxBitsToClear )               xEventGroupClearBits( xEventGroup, uxBitsToClear )
EventBits_t xEventGroupClearBits( EventGroupHandle_t xEventGroup, const EventBits_t uxBitsToClear );

#define xEventGroupGetBitsFromISR( xEventGroup )            xEventGroupGetBits( xEventGroup )
EventBits_t xEventGroupGetBits( EventGroupHandle_t xEventGroup );

#define xEventGroupSetBitsFromISR( xEventGroup, uxBitsToSet, pxHigherPriorityTaskWoken )                xEventGroupSetBits( xEventGroup, uxBitsToSet )
EventBits_t xEventGroupSetBits( EventGroupHandle_t xEventGroup, const EventBits_t uxBitsToSet );

EventBits_t xEventGroupSync( EventGroupHandle_t xEventGroup, const EventBits_t uxBitsToSet, const EventBits_t uxBitsToWaitFor, TickType_t xTicksToWait );
EventBits_t xEventGroupWaitBits( const EventGroupHandle_t xEventGroup, const EventBits_t uxBitsToWaitFor, const BaseType_t xClearOnExit, const BaseType_t xWaitForAllBits, TickType_t xTicksToWait );

#endif