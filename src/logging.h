#pragma once
#include <stdio.h>

// If you are debugging, uncomment the following line, then you can 
// use the debugf function to print debug information to the console.

// # define DEBUG 1

#ifdef DEBUG
#define __LOGGING__ AllocConsole();\
    auto _logging_stdout = freopen("CONOUT$", "w", stdout)
#define debugf(...) printf(__VA_ARGS__)
#else
#define __LOGGING__ 
#define debugf(...)
#endif


