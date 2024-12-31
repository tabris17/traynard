#include <windows.h>
#include <psapi.h>

#include "winevent.h"
#include "traymond.h"
#include "logging.h"
#include "rules.h"


DWORD GetWindowExeFileName(HWND hwnd, PTCHAR fileName, DWORD size)
{
    DWORD processId;
    if (0 == GetWindowThreadProcessId(hwnd, &processId)) {
        return FALSE;
    }
    HANDLE process = OpenProcess(PROCESS_QUERY_INFORMATION | PROCESS_VM_READ, FALSE, processId);
    if (NULL == process) {
        return FALSE;
    }
    auto result = GetModuleFileNameEx(process, NULL, fileName, size);
    CloseHandle(process);
    return result;
}

#pragma warning(push)
#pragma warning(disable:4100)
static void WINAPI DefaultWinEventProc(HWINEVENTHOOK hWinEventHook,
                                       DWORD event, HWND hwnd,
                                       LONG idObject, LONG idChild,
                                       DWORD idEventThread, DWORD dwmsEventTime)
#pragma warning(pop)
{
    if (event != EVENT_OBJECT_DESTROY) return;

    auto context = AppContext();
    context->freeWindows.erase(hwnd);
    
    if (SET_CONTAINS(context->hiddenWindows, hwnd)) {
        restoreWindow(context, 0, hwnd);
    }
}


static void WINAPI AutoHidingWinEventProc(HWINEVENTHOOK hWinEventHook, 
                                          DWORD event, HWND hwnd, 
                                          LONG idObject, LONG idChild, 
                                          DWORD idEventThread, DWORD dwmsEventTime)
{
    switch (event) {
    case EVENT_OBJECT_CREATE:
        break;
    case EVENT_OBJECT_DESTROY:
        return DefaultWinEventProc(
            hWinEventHook, 
            event, 
            hwnd, 
            idObject, 
            idChild, 
            idEventThread, 
            dwmsEventTime
        );
    case EVENT_OBJECT_SHOW:
        break;
    case EVENT_OBJECT_NAMECHANGE:
        break;
    default:
        return;
    }

    if ((OBJID_WINDOW != idObject || CHILDID_SELF != idChild) ||
        !isTopLevelWindow(hwnd) || !IsWindowVisible(hwnd)) {

        return;
    }

    auto context = AppContext();
    bool showNotification = false;
    if (!SET_CONTAINS(context->freeWindows, hwnd) && matchRule(context, hwnd, &showNotification)) {
        minimizeWindow(context, hwnd);
        if (showNotification) {
            notifyHidingWindow(context, hwnd);
        }
    }
}


void hookWinEvent(TRCONTEXT* context)
{
    context->hook = SetWinEventHook(
        EVENT_OBJECT_CREATE, 
        EVENT_OBJECT_NAMECHANGE,
        NULL, 
        context->autoHiding ? AutoHidingWinEventProc : DefaultWinEventProc,
        0, 0, 
        WINEVENT_OUTOFCONTEXT | WINEVENT_SKIPOWNPROCESS
    );
}

void unhookWinEvent(TRCONTEXT* context)
{
    if (context->hook) {
        UnhookWinEvent(context->hook);
    }
}

