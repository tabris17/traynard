unit Traynard.Page.Launcher;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls, StdCtrls, ActnList, Menus, Traynard.Page;

type

  { TPageLauncher }

  TPageLauncher = class(TFramePage, IFPObserver)
    ActionEdit: TAction;
    ActionList: TActionList;
    ButtonEdit: TButton;
    ButtonLaunch: TButton;
    ButtonPanel: TPanel;
    IconList: TImageList;
    ListView: TListView;
    LaunchMenu: TPopupMenu;
    MenuItemEmpty: TMenuItem;
    procedure ActionEditExecute(Sender: TObject);
    procedure ButtonLaunchClick(Sender: TObject);
    procedure PageActivate(Sender: TObject);
    procedure PageDeactivate(Sender: TObject);
  private
    procedure UpdateProcessList;
    procedure SetLaunchMenuItems;
    procedure LaunchMenuItemClick(Sender: TObject);
  public
    procedure Initialize; override;
    procedure FPOObservedChanged(ASender: TObject; Operation: TFPObservedOperation; Data: Pointer);
  end;

implementation

uses
  Windows, JwaWinBase, StrUtils,
  Traynard.Types, Traynard.Form.Main, Traynard.Launcher, Traynard.Settings, Traynard.Helpers;

{$R *.lfm}

{ TPageLauncher }

procedure TPageLauncher.PageActivate(Sender: TObject);
begin
  SetLaunchMenuItems;
  UpdateProcessList;               
  Launcher.Processes.FPOAttachObserver(Self);
  with ListView do if CanFocus then SetFocus;
end;

procedure TPageLauncher.ActionEditExecute(Sender: TObject);
begin
  FormMain.Navigate(piLaunchEntries);
end;

procedure TPageLauncher.ButtonLaunchClick(Sender: TObject);
var
  ClientPoint, ScreenPoint: TPoint;
begin
  ClientPoint.X := 0;
  ClientPoint.Y := ButtonLaunch.Height;
  ScreenPoint := ButtonLaunch.ClientToScreen(ClientPoint);
  LaunchMenu.PopUp(ScreenPoint.X, ScreenPoint.Y);
end;

procedure TPageLauncher.PageDeactivate(Sender: TObject);
begin
  Launcher.Processes.FPODetachObserver(Self);
  ListView.ItemIndex := -1;
end;

procedure TPageLauncher.UpdateProcessList;

  function FileTimeToStr(constref FileTime: TFileTime): string;
  var
    ALocalTime: TFileTime;
    ASysTime: TSystemTime;
    ADateTime: TDateTime;
  begin
    FileTimeToLocalFileTime(FileTime, ALocalTime);
    FileTimeToSystemTime(ALocalTime, ASysTime);
    ADateTime := SystemTimeToDateTime(ASysTime);
    Result := FormatDateTime('yyyy-mm-dd hh:nn:ss', ADateTime);
  end;

var
  Process: TLauncher.TProcess;
  SelectedPID: DWORD = 0;
  CommandLine: string;
begin
  if ListView.SelCount > 0 then SelectedPID := DWORD(PtrUInt(ListView.Selected.Data));
  ListView.BeginUpdate;
  ListView.Clear;
  IconList.Reset(1);
  for Process in Launcher.Processes do
  begin
    with ListView.Items.Add do
    begin
      Data := Pointer(PtrUInt(Process.PID));
      if SelectedPID = Process.PID then Selected := True;
      Caption := Process.Name;
      SubItems.Add(IntToStr(Process.PID));
      SubItems.Add(FileTimeToStr(Process.CreationTime));
      CommandLine := Process.Application;
      if not StartsStr(CommandLine, '"') and
         ContainsStr(CommandLine, ' ') then
        CommandLine := '"' + CommandLine + '"';
      if Process.Arguments <> '' then
        CommandLine := CommandLine + ' ' + Process.Arguments;
      SubItems.Add(CommandLine);
      ImageIndex := IconList.AddIcon(ExtractIcon(hInstance, PChar(Process.Application), 0), 0);
    end;
  end;
  ListView.EndUpdate;
end;

procedure TPageLauncher.SetLaunchMenuItems;
const
  LAUNCH_MENU_ITEM_COUNT = 1;
var
  ItemIndex: integer;
  Entry: TLaunchEntry;
  MenuItem: TMenuItem;
begin
  if LaunchMenu.Items.Count > LAUNCH_MENU_ITEM_COUNT then
  begin
    for ItemIndex := LaunchMenu.Items.Count - 1 downto LAUNCH_MENU_ITEM_COUNT do
      LaunchMenu.Items.Delete(ItemIndex);
  end;
  for ItemIndex := 0 to Launcher.Count - 1 do
  begin
    Entry := Launcher.Entries[ItemIndex];
    if not (lmManual in Entry.LaunchMethods) then Continue;
    MenuItem := TMenuItem.Create(Self);
    MenuItem.Caption := Entry.Name;
    MenuItem.Enabled := Settings.EnableLauncher;
    MenuItem.OnClick := @LaunchMenuItemClick;
    LaunchMenu.Items.Add(MenuItem);
  end;
  MenuItemEmpty.Visible := LaunchMenu.Items.Count = LAUNCH_MENU_ITEM_COUNT;
end;

procedure TPageLauncher.LaunchMenuItemClick(Sender: TObject);
begin
  if not Settings.EnableLauncher then Exit;
  try
    Launcher.Launch((Sender as TMenuItem).Caption);
  except
    on Exc: Exception do raise ERuntimeWarning.Create(Exc.Message);
  end;
end;

procedure TPageLauncher.Initialize;
begin
  inherited Initialize;
  OnPageActivate := @PageActivate;
  OnPageDeactivate := @PageDeactivate;
  IconList.AddIcon(LoadIcon(0, IDI_APPLICATION));
end;

procedure TPageLauncher.FPOObservedChanged(ASender: TObject; Operation: TFPObservedOperation; Data: Pointer);
begin
  if Operation in [ooAddItem, ooDeleteItem, ooChange] then UpdateProcessList;
end;

end.

