#pragma once

#include "traymond.h"

void hookWinEvent(TRCONTEXT* context);
void unhookWinEvent(TRCONTEXT* context);
DWORD GetWindowExeFileName(HWND hwnd, PTCHAR fileName, DWORD size);
