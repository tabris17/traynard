#include <windows.h>
#include <tchar.h>

#include "i18n.h"


bool I18n::loadLangMod()
{
    // override default language
    /* if (_tcscmp(DEFAULT_LOCALE, locale) == 0) {
        return false;
    }*/
    TCHAR fileName[MAX_PATH];
    auto fileNameLength = GetModuleFileName(NULL, fileName, MAX_PATH);
    if (fileNameLength == 0 || fileNameLength >= MAX_PATH) {
        return false;
    }
    for (int i = fileNameLength - 1; i >= 0; i--) {
        if (fileName[i] == _T("\\")[0]) {
            auto localeLen = _tcsclen(locale);
            if (i + _countof(LOCALE_DIR) + localeLen + 1 >= MAX_PATH) {
                return false;
            }
            rsize_t destSize, srcSize;
            auto offset = i + 1;
            destSize = (MAX_PATH - offset) * sizeof(TCHAR);
            srcSize = _countof(LOCALE_DIR) * sizeof(TCHAR);
            if (memcpy_s(fileName + offset, destSize, LOCALE_DIR, srcSize) != 0) {
                return false;
            }
            offset += _countof(LOCALE_DIR) - 1;
            destSize = (MAX_PATH - offset) * sizeof(TCHAR);
            srcSize = localeLen * sizeof(TCHAR);
            if (memcpy_s(fileName + offset, destSize, locale, srcSize) != 0) {
                return false;
            }
            TCHAR dotTail[] = _T(".");
            offset += localeLen;
            destSize = (MAX_PATH - offset) * sizeof(TCHAR);
            srcSize = _countof(dotTail) * sizeof(TCHAR);
            if (memcpy_s(fileName + offset, destSize, dotTail, srcSize) != 0) {
                return false;
            }
            langMod = LoadLibrary(fileName);
            break;
        }
    }
    return langMod != NULL;
}

I18n::I18n()
{
    constexpr auto MAX_CTRY = 4;
    TCHAR country[MAX_CTRY]{ NULL };
    auto ret = GetLocaleInfo(LOCALE_USER_DEFAULT, LOCALE_SISO639LANGNAME, locale, MAX_LOCALE);
    if (ret > 0) {
        if (GetLocaleInfo(LOCALE_USER_DEFAULT, LOCALE_SISO3166CTRYNAME, country, MAX_CTRY) > 0) {
            locale[ret - 1] = L'_';
            _tcscpy_s(locale + ret, MAX_LOCALE - ret, country);
        }
    }
    loadLangMod();
    for (int i = 0; i < _countof(stringTable); i++) {
        LoadString(langMod, IDS_BEGIN + i, reinterpret_cast<PTCHAR>(stringTable + i), 0);
    }
}

LPTSTR I18n::operator[](int stringId)
{
    if (stringId < IDS_BEGIN || stringId >= IDS_END) {
        return nullptr;
    }
    return stringTable[stringId - IDS_BEGIN];
}

HINSTANCE I18n::lang(HINSTANCE defaultInstance = NULL)
{
    if (langMod == NULL) {
        return defaultInstance;
    }
    return langMod;
}
