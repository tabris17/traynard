#pragma once
#include <stdio.h>

#ifdef DEBUG
#define __LOGGING__ AllocConsole();\
    auto _logging_stdout = freopen("CONOUT$", "w", stdout)
#define debugf(...) printf(__VA_ARGS__)
#else
#define __LOGGING__ 
#define debugf(...)
#endif


