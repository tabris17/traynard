#include <windows.h>
#include <windowsx.h>
#include <commctrl.h>
#include <string>

#include "resource.h"
#include "icons.h"
#include "logging.h"
#include "traymond.h"


static BOOL initDialog(HWND hwnd, IconsDialog* state)
{
    LVITEM lvi{};
    LVCOLUMN lvc{};

    auto context = state->getContext();
    auto listView = GetDlgItem(hwnd, IDC_ICON_LIST);
    auto imageList = state->getImageList();

    SetWindowLongPtr(hwnd, GWL_EXSTYLE, WS_EX_TOOLWINDOW | WS_EX_TOPMOST);
    SetWindowLongPtr(hwnd, GWLP_USERDATA, reinterpret_cast<LONG>(state));
    ListView_SetExtendedListViewStyle(listView, LVS_EX_BORDERSELECT | LVS_EX_ONECLICKACTIVATE | LVS_EX_AUTOSIZECOLUMNS | LVS_EX_FULLROWSELECT | LVS_EX_TRACKSELECT);
    ListView_SetHoverTime(listView, 0);
    ListView_SetImageList(listView, imageList, LVSIL_NORMAL);
    ListView_SetImageList(listView, imageList, LVSIL_SMALL);

    RECT rect{};
    GetClientRect(listView, &rect);
    lvc.mask = LVCF_FMT | LVCF_WIDTH | LVCF_SUBITEM;
    lvc.fmt = LVCFMT_LEFT;
    lvc.cx = rect.right;
    lvc.iSubItem = 0;
    ListView_InsertColumn(listView, 0, &lvc);

    lvi.mask = LVIF_TEXT | LVIF_IMAGE;
    lvi.cchTextMax = MAX_WINDOW_TEXT;
    for (int i = 0; i < context->iconIndex; i++) {
        TCHAR text[MAX_WINDOW_TEXT]{};
        HWND hiddenWindow = context->icons[i].window;
        GetWindowText(hiddenWindow, text, MAX_WINDOW_TEXT);
        ImageList_AddIcon(imageList, getWindowIcon(context, hiddenWindow));
        lvi.iItem = i;
        lvi.pszText = text;
        lvi.iImage = i;
        ListView_InsertItem(listView, &lvi);
    }

    return TRUE;
}


static BOOL restoreSelectedWindow(HWND hwnd)
{
    TRCONTEXT* context = IconList_GetContext(hwnd);
    int selectedIndex = IconList_GetSelectedIndex(hwnd);

    if (selectedIndex >= 0 && selectedIndex < context->iconIndex) {
        restoreWindow(context, 0, context->icons[selectedIndex].window);
        return EndDialog(hwnd, IDOK);
    }

    return FALSE;
}


static LRESULT CALLBACK DialogProc(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
    switch (uMsg) {
    case WM_COMMAND:
        switch (LOWORD(wParam)) {
        case IDCANCEL:
            return EndDialog(hwnd, IDCANCEL);
        case IDOK:
            return restoreSelectedWindow(hwnd);
        }
        break;
    case WM_NOTIFY:
        if (wParam == IDC_ICON_LIST && 
            reinterpret_cast<LPNMHDR>(lParam)->code == NM_CLICK) {

            return restoreSelectedWindow(hwnd);
        }
        break;
    case WM_NCACTIVATE:
        if (wParam == 0 && IsWindowVisible(hwnd)) {
            return EndDialog(hwnd, IDCANCEL);
        }
        break;
    case WM_INITDIALOG:
        return initDialog(hwnd, reinterpret_cast<IconsDialog*>(lParam));
    }
    return DefWindowProc(hwnd, uMsg, wParam, lParam);
}

INT_PTR showIconsDlg(TRCONTEXT *context)
{
    static bool dialogOpened = false;
    if (dialogOpened || context->iconIndex == 0) {
        return FALSE;
    }
    dialogOpened = true;
    auto state = IconsDialog(context);
    auto result = DialogBoxParam(
        context->instance,
        MAKEINTRESOURCE(IDD_ICONS),
        HWND_DESKTOP,
        (DLGPROC)DialogProc,
        (LPARAM)&state
    );
    dialogOpened = false;
    return result;
}

IconsDialog::IconsDialog(TRCONTEXT* context)
{
    this->context = context;
    auto cx = GetSystemMetrics(SM_CXSMICON), 
         cy = GetSystemMetrics(SM_CYSMICON);
    imageList = ImageList_Create(cx, cy, ILC_COLORDDB, context->iconIndex, 1);
}

IconsDialog::~IconsDialog()
{
    ImageList_Destroy(imageList);
}

TRCONTEXT* IconsDialog::getContext()
{
    return context;
}

HIMAGELIST IconsDialog::getImageList()
{
    return imageList;
}
