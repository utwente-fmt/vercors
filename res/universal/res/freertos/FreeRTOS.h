#ifndef FREERTOS_H
#define FREERTOS_H

#include "FreeRTOSConfig.h"
#include <stdint.h>
#include <stddef.h>
#include <assert.h>

#define BaseType_t int
#define UBaseType_t int
#define TickType_t int
#define TaskHandle_t int

// Prototype for task functions
// typedef void (* TaskFunction_t)( void * arg );

#ifndef pdMS_TO_TICKS
    #define pdMS_TO_TICKS( xTimeInMs )    ( ( xTimeInMs * configTICK_RATE_HZ ) / 1000U )
#endif
#ifndef pdTICKS_TO_MS
    #define pdTICKS_TO_MS( xTimeInTicks )    ( ( xTimeInTicks * 1000U ) / configTICK_RATE_HZ )
#endif
#ifndef configASSERT
    #define configASSERT( x )       assert(x != 0)
#endif

const int portMAX_DELAY;
const int pdPASS;
const int pdFAIL;
const int pdTRUE;
const int pdFALSE;

#define taskDISABLE_INTERRUPTS() vPortDisableInterrupts()
#define taskENABLE_INTERRUPTS() vPortEnableInterrupts()

void vPortDisableInterrupts();
void vPortEnableInterrupts();

#define vPortSetInterruptHandler( code, func )              vesuvISRCreate( func() )
void vesuvISRCreate( void *vesuvIGNORE );

#endif