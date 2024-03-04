// ada_function.h

#ifndef ADA_FUNCTION_H
#define ADA_FUNCTION_H
#endif

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>

// Declare the C interface to the Ada functions in Plan_Builder package

// Function declarations
void ada_function_initialize();  // Initialize the Ada package
void ada_function_cleanup();     // Clean up resources

#ifdef __cplusplus
}
#endif