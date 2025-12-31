unit Traynard.Strings;

{$mode ObjFPC}{$H+}

interface

const
  APP_NAME             = 'Traynard';
  _URL_BASE            = 'https://github.com/tabris17/traynard';
  URL_GITHUB           = _URL_BASE;
  URL_NEW_ISSUE        = _URL_BASE + '/issues/new';
  URL_USAGE            = _URL_BASE + '/wiki';
  MAINTAINER           = 'tabris17';
  LICENSE              = 'MIT';
  MSG_SYSTEM_MENU      = APP_NAME + '.SystemMenu';
  ARGUMENT_SILENT      = 'silent';
  ARGUMENT_SILENT_CHAR = 's';
  ARGUMENT_CREATE_TASK = 'create-task';
  ARGUMENT_REMOVE_TASK = 'remove-task'; 
  ARGUMENT_RUN_TASK    = 'run-task';
  NATIVE_LANGUAGE_NAME = 'English';
  NATIVE_LANGUAGE_CODE = 'en_US';
  USER32_DLL           = 'user32.dll';
  HOOK_DLL             = 'traynard.dll';
  IS_TOP_LEVEL_WINDOW  = 'IsTopLevelWindow';
  HOOK_INSTALL         = 'Install';
  HOOK_UNINSTALL       = 'Uninstall';

resourcestring
  ERROR_GET_OS_VERSION                = 'Failed to get OS version info';
  ERROR_REGISTER_SHELL_HOOK_WINDOW    = 'Failed to register shell hook window';
  ERROR_CLOSE_WINDOW                  = 'Failed to close the window';
  ERROR_SET_TOPMOST                   = 'Cannot set window topmost status';
  ERROR_FILE_PROPERTIES               = 'Cannot access file properties';
  ERROR_NO_SELECTED_WINDOW            = 'No selected window';
  ERROR_WINDOW_NOT_FOUND              = 'Window not found';
  ERROR_RESTORE_WINDOW                = 'Failed to restore window';
  ERROR_UWP_WINDOW                    = 'Unable to minimize the UWP app window';
  ERROR_PARSE_TOML_FILE               = 'Failed to parse TOML file "%s"';
  ERROR_INSTALL_HOOK                  = 'Failed to install hook';
  ERROR_UNINSTALL_HOOK                = 'Failed to uninstall hook';
  ERROR_MAX_LAUNCHES                  = 'Exceeded the maximum number of launches';

  HTML_ABOUT_VERSION     = 'Version:';
  HTML_ABOUT_LICENSE     = 'License:';
  HTML_ABOUT_MAINTAINER  = 'Maintainer:';
  HTML_ABOUT_SOURCE_CODE = 'Source code:';
  HTML_ABOUT_BUILD_DATE  = 'Build date:';
  HTML_ABOUT_COMPILER    = 'Compiler:';
  HTML_ABOUT_TITLE       = 'About';
  HTML_ABOUT_INTRO       = 'A desktop utility for minimizing any application window to the system tray, A.K.A. "notification area". ' +
                           'The project is inspired by <a href="https://github.com/fcFn/traymond">Traymond</a> and completely rewritten in Lazarus (free pascal). ';
  HTML_ABOUT_FEATURES    = 'Features';
  HTML_ABOUT_FEATURE_1   = 'Quick actions via global hotkeys';
  HTML_ABOUT_FEATURE_2   = 'Extended system menu for any window';
  HTML_ABOUT_FEATURE_3   = 'Auto-minimize windows by user rules';
  HTML_ABOUT_FEATURE_4   = 'Multi-language support';

  MENU_ITEM_MINIMIZE_TO_TRAY_ICON = 'Minimize to Tray Icon';
  MENU_ITEM_MINIMIZE_TO_TRAY_MENU = 'Minimize to Tray Menu';
  MENU_ITEM_ALWAYS_ON_TOP         = 'Always on Top';
  MENU_ITEM_RESTORE_GROUP         = 'Restore Group';

  MSG_WINDOW_MINIMIZED = 'Window minimized';
  MSG_WINDOW_RESTORED = 'Window restored';
  MSG_WINDOWS_RESTORED = '%d window(s) restored';
  MSG_WINDOWS_RESTORED_FAILED = '%0:d window(s) restored, %1:d failed';
  MSG_QUESTION_SAVE_RULE = 'Save rule to "%s"?';
  MSG_QUESTION_DELETE_RULE = 'Delete rule "%s"?';
  MSG_RULE_NAME_REQUIRED = 'Rule name is required';
  MSG_RULE_WINDOW_TITLE_REQUIRED = 'Window title is required';
  MSG_RULE_WINDOW_CLASS_REQUIRED = 'Window class is required';  
  MSG_RULE_APP_PATH_REQUIRED = 'App path is required';
  MSG_RULE_NAME_DUPLICATE = 'Rule name is duplicate';              
  MSG_RULE_TRIGGER_ON_REQUIRED = 'Trigger condition is required';
  MSG_RULE_NOTIFICATION_REQUIRED = 'Notification condition is required';

  MSG_LAUNCHER_ENTRY_NAME_REQUIRED = 'Launcher entry name is required';
  MSG_LAUNCHER_ENTRY_NAME_DUPLICATE = 'Launcher entry name is duplicate';
  MSG_LAUNCHER_ENTRY_APPLICATION_REQUIRED = 'Executable path is required';
  MSG_LAUNCHER_ENTRY_LAUNCH_METHODS_REQUIRED = 'Launch methods is required';
  MSG_QUESTION_SAVE_LAUNCHER_ENTRY = 'Save entry to "%s"?';
  MSG_QUESTION_DELETE_LAUNCHER_ENTRY = 'Delete entry "%s"?';

  MSG_MUST_RUN_AS_ADMINISTRATOR = 'Must run as administrator';

  TEXT_MENU = 'Menu';
  TEXT_ICON = 'Icon';
  TEXT_YES  = 'Yes';
  TEXT_EQUALS = 'Equals';
  TEXT_STARTS_WITH = 'Starts with';
  TEXT_ENDS_WITH = 'Ends with';
  TEXT_CONTAINS = 'Contains';
  TEXT_REGEX_MATCH = 'Regex match';
  TEXT_ANY = 'Any';

  TEXT_TRAY_MENU = 'Tray Menu';
  TEXT_TRAY_ICON = 'Tray Icon';

  TEXT_ANGLE_BRACKETED_AUTO = '<Auto>';

  TEXT_SYSTEM_MENU_TRAY_ICON = 'Add "Minimize to Tray Icon" to every window menu';
  TEXT_SYSTEM_MENU_TRAY_MENU = 'Add "Minimize to Tray Menu" to every window menu';
  TEXT_SYSTEM_MENU_TOPMOST   = 'Add "Always on Top" to every window menu';

  TEXT_NEVER = 'Never';
  TEXT_FIRST_TIME_ONLY = 'First-time only';
  TEXT_ALWAYS = 'Always';
  TEXT_FOLLOW_GLOBAL_SETTINGS = 'Follow global settings';

  TEXT_AUTOMATIC = 'Automatic';
  TEXT_MANUAL = 'Manual';

  TEXT_WINDOW_CREATION = 'Window creation';
  TEXT_WINDOW_TITLE_CHANGE = 'Window title change';
  TEXT_WINDOW_MINIMIZING = 'Window minimizing';
  TEXT_HOTKEY = 'Hotkey';
  TEXT_WINDOW_DEACTIVATED = 'Window deactivated';
  TEXT_BRACKETED_NO_SET = '(no set)';

  HOTKEY_MINIMIZE_TO_ICON        = 'Minimize foreground window to tray icon';
  HOTKEY_MINIMIZE_TO_MENU        = 'Minimize foreground window to tray menu';
  HOTKEY_RESTORE_LAST_WINDOW     = 'Restore the last window';
  HOTKEY_RESTORE_ALL             = 'Restore all windows';
  HOTKEY_OPEN_CLOSE              = 'Open/Close main window';
  HOTKEY_TOGGLE_TOPMOST          = 'Toggle foreground window always on top';
  HOTKEY_TOGGLE_AUTOMINIMIZE     = 'Toggle auto-minimize to tray';
  HOTKEY_TOGGLE_RULES            = 'Toggle custom rules';

implementation

end.

