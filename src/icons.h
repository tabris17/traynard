#include <windows.h>
#include <commctrl.h>

#include "traymond.h"

#define IconList_GetSelectedIndex(hIconsDialog) \
        ListView_GetNextItem(GetDlgItem(hIconsDialog, IDC_ICON_LIST), -1, LVNI_SELECTED)

#define IconList_GetContext(hIconsDialog) \
        reinterpret_cast<IconsDialog*>(GetWindowLongPtr(hIconsDialog, GWLP_USERDATA))->getContext()


class IconsDialog final {
private:
    TRCONTEXT* context;
    HIMAGELIST imageList;
public:
    IconsDialog(TRCONTEXT* context);
    ~IconsDialog();
    TRCONTEXT* getContext();
    HIMAGELIST getImageList();
};

INT_PTR showIconsDlg(TRCONTEXT* context);
