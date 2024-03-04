// ada_function.c

#include "ada_function.h"
#include "plan_builder.h"  // Include the Ada package spec file

// Initialize the Ada package
void ada_function_initialize() {
    plan_builder__initialize();
}

// Clean up resources
void ada_function_cleanup() {
    plan_builder__finalize();
}

// Implementation of the C interface to Ada functions
int main() {
    ada_function_initialize();
    ada_function_cleanup();
    return 0;
}