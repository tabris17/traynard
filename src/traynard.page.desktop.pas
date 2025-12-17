unit Traynard.Page.Desktop;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls, Menus, StdCtrls, ActnList, Buttons,
  Traynard.Page, Traynard.Window, Traynard.Types, Traynard.Helpers.WindowList;

type

  TListViewSubItemIndex = (lvsiiHandle = 0, lvsiiClass, lvsiiPID, lvsiiPath);
  TListViewColumnIndex = (lvciName = 0, lvciHandle, lvciClass, lvciPID, lvciPath);

  { TPageDesktop }

  TPageDesktop = class(TFramePage, IFPObserver)
    ActionAutoTrayMenu: TAction;
    ActionAutoTrayIcon: TAction;
    ActionTrayIcon: TAction;
    ActionFlash: TAction;
    ActionRefresh: TAction;
    ActionProperties: TAction;
    ActionTopmost: TAction;
    ActionLocate: TAction;
    ActionCopyEntireRow: TAction;
    ActionCopyPath: TAction;
    ActionCopyClass: TAction;
    ActionCopyName: TAction;
    ActionClose: TAction;
    ActionNewRule: TAction;
    ActionTrayMenu: TAction;
    ActionList: TActionList;
    ButtonRefresh: TButton;
    ButtonMinimize: TButton;
    ButtonRule: TButton;
    IconList: TImageList;
    IdleTimer: TIdleTimer;
    LabelManaged: TLabel;
    MenuItemAutoIcon: TMenuItem;
    MenuItemAutoMenu: TMenuItem;
    MenuItemAutoMenu1: TMenuItem;
    MenuItemAutoIcon1: TMenuItem;
    MenuItemToMenu1: TMenuItem;
    MenuItemToIcon1: TMenuItem;
    MenuItemToIcon: TMenuItem;
    MenuItemToMenu: TMenuItem;
    MenuItemFlash: TMenuItem;
    MenuItemRefresh: TMenuItem;
    MinimizeMenu: TPopupMenu;
    Separator3: TMenuItem;
    MenuItemProperties: TMenuItem;
    MenuItemTopmost: TMenuItem;
    MenuItemLocate: TMenuItem;
    MenuItemCopyName: TMenuItem;
    MenuItemCopyClass: TMenuItem;
    MenuItemCopyPath: TMenuItem;
    MenuItemCopyEntireRow: TMenuItem;
    MenuItemCopy: TMenuItem;
    MenuItemRule: TMenuItem;
    MenuItemClose: TMenuItem;
    MenuItemMinimize: TMenuItem;
    ListView: TListView;
    ButtonPanel: TPanel;
    Separator1: TMenuItem;
    ListViewMenu: TPopupMenu;
    Separator2: TMenuItem;
    Separator4: TMenuItem;
    Separator5: TMenuItem;
    Separator6: TMenuItem;
    Separator7: TMenuItem;
    ShapeColor: TShape;
    procedure ActionAutoTrayIconExecute(Sender: TObject);
    procedure ActionAutoTrayMenuExecute(Sender: TObject);
    procedure ActionFlashExecute(Sender: TObject);
    procedure ActionTrayIconExecute(Sender: TObject);
    procedure ActionRefreshExecute(Sender: TObject);
    procedure ActionLocateExecute(Sender: TObject);
    procedure ActionCloseExecute(Sender: TObject);
    procedure ActionCopyClassExecute(Sender: TObject);
    procedure ActionCopyEntireRowExecute(Sender: TObject);
    procedure ActionCopyNameExecute(Sender: TObject);
    procedure ActionCopyPathExecute(Sender: TObject);
    procedure ActionPropertiesExecute(Sender: TObject);
    procedure ActionNewRuleExecute(Sender: TObject);
    procedure ActionTopmostExecute(Sender: TObject);
    procedure ActionTrayMenuExecute(Sender: TObject);
    procedure ButtonMinimizeClick(Sender: TObject);
    procedure IdleTimerTimer(Sender: TObject);
    procedure ListViewCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure PageActivate(Sender: TObject);
    procedure PageDeactivate(Sender: TObject);
    procedure ListViewSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
  private
    procedure MinimizeTo(Position: TTrayPosition);
  public
    procedure Initialize; override;
    procedure FPOObservedChanged(ASender: TObject; Operation: TFPObservedOperation; Data: Pointer);
  end;


implementation

uses
  Clipbrd, Windows, LazFileUtils, fpTemplate, CommCtrl, JwaWinUser, shlobj,
  Traynard.Helpers, Traynard.Strings, Traynard.Page.Rules, Traynard.Form.Main;

{$R *.lfm}

{ TPageDesktop }

procedure TPageDesktop.PageActivate(Sender: TObject);
begin
  WindowManager.RefreshDesktop;
  ListView.UpdateWindowList(WindowManager.Desktop, IconList);
  WindowManager.Desktop.FPOAttachObserver(Self);
  with ListView do if CanFocus then SetFocus;
end;

procedure TPageDesktop.IdleTimerTimer(Sender: TObject);
begin
  ListView.UpdateWindowList(WindowManager.Desktop, IconList);
  IdleTimer.Enabled := False;
end;

procedure TPageDesktop.ListViewCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
var
  Window: TWindow;
begin
  if WindowManager.Desktop.Find(HWND(Item.Data), Window) and WindowManager.IsAutoMinimizeWindow(Window.Handle) then
    ListView.Canvas.Brush.Color := COLOR_MANAGED_WINDOW;
end;

procedure TPageDesktop.ActionTrayMenuExecute(Sender: TObject);
begin
  MinimizeTo(tpMenu);
end;

procedure TPageDesktop.ButtonMinimizeClick(Sender: TObject);
var
  ClientPoint, ScreenPoint: TPoint;
begin
  ClientPoint.X := 0;
  ClientPoint.Y := ButtonMinimize.Height;
  ScreenPoint := ButtonMinimize.ClientToScreen(ClientPoint);
  MinimizeMenu.PopUp(ScreenPoint.X, ScreenPoint.Y);
end;

procedure TPageDesktop.ActionCloseExecute(Sender: TObject);
var
  Window: TWindow;
begin
  Window := ListView.GetSelectedWindow(WindowManager.Desktop);
  if not PostMessage(Window.Handle, WM_CLOSE, 0, 0) then
    raise ERuntimeWarning.Create(ERROR_CLOSE_WINDOW, GetLastErrorMsg);
end;

procedure TPageDesktop.ActionLocateExecute(Sender: TObject);
var
  FilePath: string;
  DirPath: unicodestring;
begin
  FilePath := ListView.Selected.SubItems[integer(lvsiiPath)];
  DirPath := unicodestring(ExtractFilePath(FilePath));
  try
    ExecuteProcess('explorer.exe', Format('/select,"%s"', [FilePath]));
  except
    on EOSError do ShellExecuteW(0, 'open', PWideChar(DirPath), nil, nil, SW_SHOWNORMAL);
  end;
end;

procedure TPageDesktop.ActionRefreshExecute(Sender: TObject);
begin
  WindowManager.RefreshDesktop;
  ListView.UpdateWindowList(WindowManager.Desktop, IconList);
end;

procedure TPageDesktop.ActionFlashExecute(Sender: TObject);
var
  FlashInfo: FLASH_INFO;
  Window: TWindow;
begin
  Window := ListView.GetSelectedWindow(WindowManager.Desktop);
  FlashInfo.cbSize := SizeOf(FlashInfo);
  FlashInfo.dwFlags := FLASHW_ALL;
  FlashInfo.hwnd := Window.Handle;
  FlashInfo.uCount := 5;
  FlashInfo.dwTimeout := 0;
  FlashWindowEx(FlashInfo);
end;

procedure TPageDesktop.ActionAutoTrayIconExecute(Sender: TObject);
var
  Window: TWindow;
begin
  Window := ListView.GetSelectedWindow(WindowManager.Desktop);
  try
    if ActionAutoTrayIcon.Checked then
    begin
      WindowManager.DisableWindowAutoMinimize(Window.Handle);
      ActionAutoTrayIcon.Checked := False;
    end
    else
    begin
      WindowManager.EnableWindowAutoMinimize(Window.Handle, tpIcon);
      ActionAutoTrayIcon.Checked := True;
      ActionAutoTrayMenu.Checked := False;
    end;
  except
    on Exc: TWindowManager.Exception do
      raise ERuntimeWarning.Create(Exc.Message);
  end;
end;

procedure TPageDesktop.ActionAutoTrayMenuExecute(Sender: TObject);
var
  Window: TWindow;
begin
  Window := ListView.GetSelectedWindow(WindowManager.Desktop);
  try
    if ActionAutoTrayMenu.Checked then
    begin
      WindowManager.DisableWindowAutoMinimize(Window.Handle);
      ActionAutoTrayMenu.Checked := False;
    end
    else
    begin
      WindowManager.EnableWindowAutoMinimize(Window.Handle, tpMenu);;
      ActionAutoTrayIcon.Checked := False;
      ActionAutoTrayMenu.Checked := True;
    end;
  except
    on Exc: TWindowManager.Exception do
      raise ERuntimeWarning.Create(Exc.Message);
  end;
end;

procedure TPageDesktop.ActionTrayIconExecute(Sender: TObject);
begin
  MinimizeTo(tpIcon);
end;

procedure TPageDesktop.ActionCopyClassExecute(Sender: TObject);
begin
  Clipboard.AsText := ListView.Selected.SubItems[integer(lvsiiClass)];
end;

procedure TPageDesktop.ActionCopyEntireRowExecute(Sender: TObject);
var
  StringList: TStringList;
  Plaintext, HtmlSource: string;
  SelectedItem: TListItem;
  HtmlTmpl: TFPTemplate;
  TmplParams: TFPTemplateParams;
  ColName, ColHandle, ColClass, ColPID, ColPath: string;
  ValName, ValHandle, ValClass, ValPID, ValPath: string;
begin
  SelectedItem:=ListView.Selected;

  ColName:=ListView.Column[integer(lvciName)].Caption;
  ColHandle:=ListView.Column[integer(lvciHandle)].Caption;
  ColClass:=ListView.Column[integer(lvciClass)].Caption;
  ColPID:=ListView.Column[integer(lvciPID)].Caption;
  ColPath:=ListView.Column[integer(lvciPath)].Caption;
  ValName:=SelectedItem.Caption;
  ValHandle:=SelectedItem.SubItems[integer(lvsiiHandle)];
  ValClass:=SelectedItem.SubItems[integer(lvsiiClass)];
  ValPID:=SelectedItem.SubItems[integer(lvsiiPID)];
  ValPath:=SelectedItem.SubItems[integer(lvsiiPath)];

  HtmlTmpl:=TFPTemplate.Create;
  try
    HtmlTmpl.Template:=
      '<table>'+
        '<tr>'+
          '<th>{{ColName}}</th>'+ 
          '<th>{{ColHandle}}</th>'+
          '<th>{{ColClass}}</th>'+
          '<th>{{ColPID}}</th>'+
          '<th>{{ColPath}}</th>'+
        '</tr>'+
        '<tr>'+
          '<td>{{Name}}</td>'+  
          '<th>{{Handle}}</th>'+
          '<td>{{Class}}</td>'+
          '<td>{{PID}}</td>'+
          '<td>{{Path}}</td>'+
        '</tr>'+
      '</table>';
    TmplParams:=TFPTemplateParams.Create;
    try
      TmplParams.AddPair('ColName', ColName); 
      TmplParams.AddPair('ColHandle', ColHandle);
      TmplParams.AddPair('ColClass', ColClass);
      TmplParams.AddPair('ColPID', ColPID);
      TmplParams.AddPair('ColPath', ColPath);
      TmplParams.AddPair('Name', ValName);
      TmplParams.AddPair('Handle', ValHandle);
      TmplParams.AddPair('Class', ValClass);
      TmplParams.AddPair('PID', ValPID);
      TmplParams.AddPair('Path', ValPath);
      HtmlTmpl.OnGetParam:=@TmplParams.GetParam;
      HtmlSource:=HtmlTmpl.GetContent;
    finally
      TmplParams.Free;
    end;
  finally
    HtmlTmpl.Free;
  end;
  StringList:=TStringList.Create;
  try
    StringList.AddPair(ColName, ValName);
    StringList.AddPair(ColHandle, ValHandle);
    StringList.AddPair(ColClass, ValClass);
    StringList.AddPair(ColPID, ValPID);
    StringList.AddPair(ColPath, ValPath);
    Plaintext:=StringList.Text;
  finally
    StringList.Free;
  end;
  Clipboard.SetAsHtml(HtmlSource, Plaintext);
end;

procedure TPageDesktop.ActionCopyNameExecute(Sender: TObject);
begin
  Clipboard.AsText:=ListView.Selected.Caption;
end;

procedure TPageDesktop.ActionCopyPathExecute(Sender: TObject);
begin
  Clipboard.AsText:=ListView.Selected.SubItems[integer(lvsiiPath)];
end;

procedure TPageDesktop.ActionPropertiesExecute(Sender: TObject);
var
  FilePath: UnicodeString;
begin
  FilePath := UnicodeString(ListView.Selected.SubItems[integer(lvsiiPath)]);
  if not SHObjectProperties(Parent.Handle, SHOP_FILEPATH, PWideChar(FilePath), nil) then
    raise ERuntimeWarning.Create(ERROR_FILE_PROPERTIES, GetLastErrorMsg);
end;

procedure TPageDesktop.ActionNewRuleExecute(Sender: TObject);
var
  Window: TWindow;
begin
  Window := ListView.GetSelectedWindow(WindowManager.Desktop);
  (FormMain.Navigate(piRules) as TPageRules).NewRule(
    Sender,
    ExtractFileNameOnly(Window.AppPath),
    Window.Text,
    Window.ClassName,
    Window.AppPath
  );
end;

procedure TPageDesktop.ActionTopmostExecute(Sender: TObject);
var
  Window: TWindow;
  IsTopmostChecked: Boolean;
begin
  Window := ListView.GetSelectedWindow(WindowManager.Desktop);
  IsTopmostChecked := ActionTopMost.Checked;
  try
    WindowManager.SetWindowAlwaysOnTop(Window.Handle, not IsTopmostChecked);
    ActionTopMost.Checked := not IsTopmostChecked;
  except
    on Exc: TWindowManager.Exception do
      raise ERuntimeWarning.Create(ERROR_SET_TOPMOST, Exc.Message);
  end;
end;

procedure TPageDesktop.PageDeactivate(Sender: TObject);
begin
  WindowManager.Desktop.FPODetachObserver(Self);
  ListView.ItemIndex := -1;
end;

procedure TPageDesktop.ListViewSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
var
  Window: TWindow;
  PathAvailable: boolean;
  TrayPosition: TTrayPosition;
begin
  ActionTrayMenu.Enabled := Selected;
  ActionTrayIcon.Enabled := Selected;
  ActionFlash.Enabled := Selected;
  ActionNewRule.Enabled := Selected;
  ActionClose.Enabled := Selected;
  ActionCopyName.Enabled := Selected;
  ActionCopyClass.Enabled := Selected;
  ActionCopyPath.Enabled := Selected;
  ActionCopyEntireRow.Enabled := Selected;
  PathAvailable := Selected and (Item.SubItems.Count > integer(lvsiiPath)) and (Item.SubItems[integer(lvsiiPath)] <> '');
  ActionLocate.Enabled := PathAvailable;
  ActionProperties.Enabled := PathAvailable;
  MenuItemCopy.Enabled := Selected;
  MenuItemMinimize.Enabled := Selected;
  ButtonMinimize.Enabled := Selected;
  ActionTopmost.Enabled := Selected;
  ActionTopmost.Checked := False;
  ActionAutoTrayIcon.Enabled := Selected;
  ActionAutoTrayIcon.Checked := False;
  ActionAutoTrayMenu.Enabled := Selected;
  ActionAutoTrayMenu.Checked := False;
  if Selected and WindowManager.Desktop.Find(HWND(Item.Data), Window) then
  begin
    Window.Renew;
    if (Window.ExStyle and WS_EX_TOPMOST) <> 0 then ActionTopmost.Checked := True;
    if WindowManager.IsAutoMinimizeWindow(Window.Handle, TrayPosition) then
    begin
      case TrayPosition of
        tpIcon: ActionAutoTrayIcon.Checked := True;
        tpMenu: ActionAutoTrayMenu.Checked := True;
      end;
    end;
  end;
end;

procedure TPageDesktop.MinimizeTo(Position: TTrayPosition);
var
  Window: TWindow;
  WindowText: string;
begin
  Window := ListView.GetSelectedWindow(WindowManager.Desktop);
  WindowText := Window.Text;
  try
    WindowManager.MinimizeWindow(Window.Handle, Position);
  except
    on Exc: TWindowManager.Exception do
      raise ERuntimeWarning.Create(Exc.Message);
  end;
  FormMain.Popup(MSG_WINDOW_MINIMIZED, WindowText, ptInformation);
end;

procedure TPageDesktop.Initialize;
begin
  inherited Initialize;
  OnPageActivate := @PageActivate;
  OnPageDeactivate := @PageDeactivate;
  IconList.AddIcon(LoadIcon(0, IDI_APPLICATION));
end;

procedure TPageDesktop.FPOObservedChanged(ASender: TObject; Operation: TFPObservedOperation; Data: Pointer);
begin
  if Operation in [ooAddItem, ooDeleteItem, ooChange] then IdleTimer.Enabled := True;
end;

end.

