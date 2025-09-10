program Traynard;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, SysUtils,
  Traynard.Form.Background,
  Traynard.Form.Main,
  Traynard.Strings,
  Traynard.Window,
  Traynard.Hotkey,
  Traynard.Rule,
  Traynard.Notification,
  Traynard.Tray;

{$R *.res}

begin
  {$IF Declared(UseHeapTrace)}
  DeleteFile('heap.trc');
  SetHeapTraceOutput('heap.trc');
  {$ENDIF}

  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Title := APP_NAME;
  Application.ShowMainForm := False;
  Application.Initialize;
  Application.CreateForm(TFormBackground, FormBackground);
  NotificationManager := TNotificationManager.Create(Application.MainForm);
  RuleManager := TRuleManager.Create(Application.MainForm);
  WindowManager := TWindowManager.Create(Application.MainForm);
  TrayManager := TTrayManager.Create(Application.MainForm);
  HotkeyManager := THotkeyManager.Create(Application.MainForm);
  if not Application.HasOption(ARGUMENT_SILENT_CHAR, ARGUMENT_SILENT) and
     not Application.Terminated then
  begin
    Application.CreateForm(TFormMain, FormMain);
    FormMain.Show;
  end;
  Application.Run;
end.

