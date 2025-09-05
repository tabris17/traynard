unit Traynard.Form.Background;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ActnList, Menus, UniqueInstance,
  Traynard.Types;

type

  { TFormBackground }

  TFormBackground = class(TForm, IFPObserver)
    ActionClose: TAction;
    ActionApplyRules: TAction;
    ActionAutoMinimize: TAction;
    ActionRestoreLast: TAction;
    ActionRestoreAll: TAction;
    ActionExit: TAction;
    ActionOpen: TAction;
    ActionList: TActionList;
    ApplicationProperties: TApplicationProperties;
    MenuItemUseRules: TMenuItem;
    MenuItemAutoMinimize: TMenuItem;
    MenuItemRestoreAll: TMenuItem;
    MenuItemRestoreLast: TMenuItem;
    MenuItemOpen: TMenuItem;
    MenuItemExit: TMenuItem;
    Separator1: TMenuItem;
    Separator2: TMenuItem;
    Separator3: TMenuItem;
    Separator4: TMenuItem;
    TrayMenu: TPopupMenu;
    TrayIcon: TTrayIcon;
    UniqueInstance: TUniqueInstance;
    procedure ActionAutoMinimizeExecute(Sender: TObject);
    procedure ActionCloseExecute(Sender: TObject);
    procedure ActionExitExecute(Sender: TObject);
    procedure ActionOpenExecute(Sender: TObject);
    procedure ActionRestoreAllExecute(Sender: TObject);
    procedure ActionRestoreLastExecute(Sender: TObject);
    procedure ActionApplyRulesExecute(Sender: TObject);
    procedure ApplicationPropertiesException(Sender: TObject; E: Exception);
    procedure FormClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure TrayIconDblClick(Sender: TObject);
    procedure TrayMenuPopup(Sender: TObject);
    procedure UniqueInstanceOtherInstance(Sender: TObject; ParamCount: Integer; const Parameters: array of String);
  private

  public
    procedure Notify(const Title, Message: string; const Timeout: Integer = 0);
    procedure FPOObservedChanged(ASender: TObject; Operation: TFPObservedOperation; Data: Pointer);
  end;

var
  FormBackground: TFormBackground;

implementation

uses Traynard.Form.Popup, Traynard.Form.Main, Traynard.Strings, Traynard.Window, Traynard.Settings;

{$R *.lfm}

{ TFormBackground }

procedure TFormBackground.ApplicationPropertiesException(Sender: TObject; E: Exception);
var
  PopupType: TPopupType = ptNone;
begin
  if (E is ERuntimeException) then
  begin
    if E is ERuntimeWarning then
      PopupType := ptWarning
    else if E is ERuntimeError then
      PopupType := ptError
    else if E is ERuntimeInfo then
      PopupType := ptInformation;
    with E as ERuntimeException do
    begin
      if Assigned(FormMain) then
        FormMain.Popup(Title, Message, PopupType)
      else
        Notify(Title, Message);
    end;
  end
  else ShowMessage(E.ToString);
end;

procedure TFormBackground.ActionOpenExecute(Sender: TObject);
begin
  if not Assigned(FormMain) then
    Application.CreateForm(TFormMain, FormMain);
  if not FormMain.Visible then
    FormMain.Show;
  FormMain.BringToFront;
end;

procedure TFormBackground.ActionRestoreAllExecute(Sender: TObject);
begin
  WindowManager.TryRestoreAllWindows;
end;

procedure TFormBackground.ActionRestoreLastExecute(Sender: TObject);
var
  Window: TWindow;
begin
  Window := WindowManager.Tray.LastWindow;
  try
    WindowManager.RestoreWindow(Window.Handle);
  except
    on Exc: TWindowManager.Exception do;
  end;
end;

procedure TFormBackground.ActionApplyRulesExecute(Sender: TObject);
begin
  Settings.ApplyRules := not ActionApplyRules.Checked;
end;

procedure TFormBackground.ActionExitExecute(Sender: TObject);
begin
  //Application.Terminate;
  Close;
end;

procedure TFormBackground.ActionAutoMinimizeExecute(Sender: TObject);
begin
  Settings.AutoMinimize := not ActionAutoMinimize.Checked;
end;

procedure TFormBackground.ActionCloseExecute(Sender: TObject);
begin
  if Assigned(FormMain) then FormMain.Close;
end;

procedure TFormBackground.FormClick(Sender: TObject);
begin
  ActionOpen.Execute;
end;

procedure TFormBackground.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if Assigned(FormMain) then
  begin
    FormMain.Close;
    CanClose := FormMain.Closed;
  end
  else
    CanClose := True;
end;

procedure TFormBackground.FormCreate(Sender: TObject);
begin
  UniqueInstance.Identifier := APP_NAME;
  TrayIcon.Hint := APP_NAME;
  TrayIcon.Icon := Application.Icon;
  PopupMenu := TrayMenu;
end;

procedure TFormBackground.TrayIconDblClick(Sender: TObject);
begin
  ActionOpen.Execute;
end;

procedure TFormBackground.TrayMenuPopup(Sender: TObject);
var
  HasTrayWindow: boolean = False;
begin
  HasTrayWindow := WindowManager.Tray.WindowCount > 0;
  Separator1.Visible := HasTrayWindow;
  ActionRestoreAll.Enabled := HasTrayWindow;
  ActionRestoreLast.Enabled := HasTrayWindow;
  ActionAutoMinimize.Checked := Settings.AutoMinimize; 
  ActionApplyRules.Checked := Settings.ApplyRules;
end;

procedure TFormBackground.UniqueInstanceOtherInstance(Sender: TObject; ParamCount: Integer; const Parameters: array of String);
begin
  ActionOpen.Execute;
end;

procedure TFormBackground.Notify(const Title, Message: string; const Timeout: Integer);
begin
  with TrayIcon do
  begin
    BalloonTitle := Title;
    BalloonHint := Message;
    if Timeout > 0 then BalloonTimeout := Timeout;
    ShowBalloonHint;
  end;
end;

procedure TFormBackground.FPOObservedChanged(ASender: TObject; Operation: TFPObservedOperation; Data: Pointer);
begin

end;

end.

