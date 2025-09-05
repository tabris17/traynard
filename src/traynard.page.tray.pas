unit Traynard.Page.Tray;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls, ActnList, StdCtrls, Menus,
  Traynard.Page, Traynard.Window, Traynard.Helpers.WindowList;

type

  { TPageTray }

  TPageTray = class(TFramePage, IFPObserver)
    ActionRestoreLast: TAction;
    ActionRestoreAll: TAction;
    ActionRestore: TAction;
    ActionList: TActionList;
    ButtonRestore: TButton;
    ButtonPanel: TPanel;
    ButtonRestoreAll: TButton;
    IconList: TImageList;
    IdleTimer: TIdleTimer;
    ListView: TListView;
    ListViewMenu: TPopupMenu;
    MenuItemRestoreLast: TMenuItem;
    MenuItemRestoreAll: TMenuItem;
    MenuItemRestore: TMenuItem;
    procedure ActionRestoreAllExecute(Sender: TObject);
    procedure ActionRestoreExecute(Sender: TObject);
    procedure ActionRestoreLastExecute(Sender: TObject);
    procedure IdleTimerTimer(Sender: TObject);
    procedure ListViewSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure PageActivate(Sender: TObject);
    procedure PageDeactivate(Sender: TObject);
  public
    procedure Initialize; override;
    procedure FPOObservedChanged(ASender: TObject; Operation: TFPObservedOperation; Data: Pointer);
  end;

implementation

uses
  Windows, Traynard.Helpers, Traynard.Types, Traynard.Strings, Traynard.Form.Main;

{$R *.lfm}

{ TPageTray }

procedure TPageTray.IdleTimerTimer(Sender: TObject);
var
  HasItem: boolean;
begin
  ListView.UpdateWindowList(WindowManager.Tray, IconList);
  IdleTimer.Enabled := False;
  HasItem := ListView.Items.Count > 0;
  ActionRestoreAll.Enabled := HasItem;
  ActionRestoreLast.Enabled := HasItem;
end;

procedure TPageTray.ListViewSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  ActionRestore.Enabled := Selected;
end;

procedure TPageTray.ActionRestoreExecute(Sender: TObject);
var
  Window: TWindow;
  WindowText: string;
begin
  Window := ListView.GetSelectedWindow(WindowManager.Tray);
  WindowText := Window.Text;
  try
    WindowManager.RestoreWindow(Window.Handle);
  except
    on Exc: TWindowManager.Exception do
      raise ERuntimeWarning.Create(Exc.Message);
  end;
  FormMain.Popup(MSG_WINDOW_RESTORED, WindowText, ptInformation);
end;

procedure TPageTray.ActionRestoreLastExecute(Sender: TObject);
var
  Window: TWindow;
  WindowText: String;
begin
  Window := WindowManager.Tray.LastWindow;
  WindowText := Window.Text;
  try
    WindowManager.RestoreWindow(Window.Handle);
  except
    on Exc: TWindowManager.Exception do
      raise ERuntimeWarning.Create(Exc.Message);
  end;
  FormMain.Popup(MSG_WINDOW_RESTORED, WindowText, ptInformation);
end;

procedure TPageTray.ActionRestoreAllExecute(Sender: TObject);
var
  Restored, Failed, Total: integer;
begin
  Total := WindowManager.Tray.WindowCount;
  Restored := WindowManager.TryRestoreAllWindows;
  Failed := Total - Restored;
  FormMain.Popup('', specialize IfThen<string>(
    Failed > 0,
    Format(MSG_WINDOWS_RESTORED_FAILED, [Restored, Failed]),
    Format(MSG_WINDOWS_RESTORED, [Restored])
  ), ptInformation);
end;

procedure TPageTray.PageActivate(Sender: TObject);
var
  HasItem: boolean;
begin
  ListView.UpdateWindowList(WindowManager.Tray, IconList);
  HasItem := ListView.Items.Count > 0;
  ActionRestoreAll.Enabled := HasItem;
  ActionRestoreLast.Enabled := HasItem;
  WindowManager.Tray.FPOAttachObserver(Self);
  with ListView do if CanFocus then SetFocus;
end;

procedure TPageTray.PageDeactivate(Sender: TObject);
begin
  WindowManager.Tray.FPODetachObserver(Self);
  ListView.ItemIndex := -1;
end;

procedure TPageTray.Initialize;
begin
  inherited Initialize;
  OnPageActivate := @PageActivate;
  OnPageDeactivate := @PageDeactivate;
  IconList.AddIcon(LoadIcon(0, IDI_APPLICATION));
end;

procedure TPageTray.FPOObservedChanged(ASender: TObject; Operation: TFPObservedOperation; Data: Pointer);
begin
  if Operation in [ooAddItem, ooDeleteItem, ooChange] then IdleTimer.Enabled := True;
end;

end.

