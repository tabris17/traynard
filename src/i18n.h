#include "resource.h"

#ifndef I18N_H
#define I18N_H
#define MAX_LOCALE 10
#define DEFAULT_LOCALE _T("zh_CN")
#define LOCALE_DIR _T("locale\\")

class I18n {
private:
    TCHAR locale[MAX_LOCALE]{};
    LPTSTR stringTable[IDS_MAX_SIZE]{};
    HINSTANCE langMod = NULL;
    bool loadLangMod();
public:
    I18n();
    LPTSTR operator[](int stringId);
    HINSTANCE lang(HINSTANCE defaultInstance);
};

#endif
