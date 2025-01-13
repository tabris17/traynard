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
            constexpr TCHAR dotTail[] = _T(".");
            fileName[i + 1] = _T("")[0];
            if (
                _tcsncat_s(fileName, MAX_PATH, LOCALE_DIR, _countof(LOCALE_DIR)) != 0 ||
                _tcsncat_s(fileName, MAX_PATH, locale, MAX_LOCALE) != 0 ||
                _tcsncat_s(fileName, MAX_PATH, dotTail, _countof(dotTail))
            ) {
                return false;
            }
            langMod = LoadLangMod(fileName);
            if (langMod) {
                return true;
            }
            fileName[i + 1] = _T("")[0];
            if (
                _tcsncat_s(fileName, MAX_PATH, LOCALE_DIR, _countof(LOCALE_DIR)) != 0 ||
                _tcsncat_s(fileName, MAX_PATH, fallback, MAX_LOCALE) != 0 ||
                _tcsncat_s(fileName, MAX_PATH, dotTail, _countof(dotTail))
            ) {
                return false;
            }
            langMod = LoadLangMod(fileName);
            if (langMod) {
                return true;
            }
            break;
        }
    }
    return false;
}

bool I18n::getLocale()
{
    constexpr auto MAX_CTRY = 4;
    TCHAR country[MAX_CTRY]{ NULL };
    if (GetLocaleInfo(LOCALE_USER_DEFAULT, LOCALE_SISO639LANGNAME, fallback, MAX_LOCALE) == 0) {
        return false;
    }
    if (GetLocaleInfo(LOCALE_USER_DEFAULT, LOCALE_SISO3166CTRYNAME, country, MAX_CTRY) == 0) {
        return false;
    }
    _stprintf_s(locale, _T("%s_%s"), fallback, country);
    return true;
}

I18n::I18n()
{
    if (getLocale()) {
        loadLangMod();
    }

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
