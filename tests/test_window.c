#include <windows.h>
#include <time.h>
#include <stdio.h>

#define IDT_TIMER1 1

LRESULT CALLBACK WndProc(HWND hwnd, UINT msg, WPARAM wParam, LPARAM lParam) {
    char buffer[64];

    switch (msg) {
    case WM_CREATE:
        SetTimer(hwnd, IDT_TIMER1, 1000, NULL);
        return 0;

    case WM_TIMER:
        if (wParam == IDT_TIMER1) {
            time_t now = time(NULL);
            struct tm *lt = localtime(&now);
            snprintf(buffer, sizeof(buffer), "%d", lt->tm_sec);
            SetWindowTextA(hwnd, buffer);
        }
        return 0;

    case WM_DESTROY:
        KillTimer(hwnd, IDT_TIMER1);
        PostQuitMessage(0);
        return 0;
    }
    return DefWindowProc(hwnd, msg, wParam, lParam);
}

int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance,
                   LPSTR lpCmdLine, int nCmdShow) {
    const char CLASS_NAME[] = "SimpleTimerWindow";
    WNDCLASS wc = {0};

    wc.lpfnWndProc   = WndProc;
    wc.hInstance     = hInstance;
    wc.lpszClassName = CLASS_NAME;

    RegisterClass(&wc);

    HWND hwnd = CreateWindowEx(
        0,
        CLASS_NAME,
        "",
        WS_OVERLAPPEDWINDOW,
        CW_USEDEFAULT, CW_USEDEFAULT, 400, 200,
        NULL, NULL, hInstance, NULL
    );

    if (hwnd == NULL) {
        return 0;
    }

    ShowWindow(hwnd, nCmdShow);

    MSG msg = {0};
    while (GetMessage(&msg, NULL, 0, 0)) {
        TranslateMessage(&msg);
        DispatchMessage(&msg);
    }

    return 0;
}
