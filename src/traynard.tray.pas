unit Traynard.Tray;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Menus, ExtCtrls, Generics.Collections, Windows,
  Traynard.Window;

type

  { TTrayIcon }

  TTrayIcon = class(ExtCtrls.TTrayIcon)
  private
    procedure TrayIconClick(Sender: TObject);
  public
    constructor Create(TheOwner: TComponent); override;
  end;

  { TGroupedTrayIcon }

  TGroupedTrayIcon = class(TTrayIcon)
  private
    FName: string;
    FPopUpMenu: TPopUpMenu;
    FRestoreGroupMenuItem: Menus.TMenuItem;
    procedure TrayIconClick(Sender: TObject);
    procedure RestoreWindowMenuItemClick(Sender: TObject);
    procedure RestoreGroupMenuItemClick(Sender: TObject);
  public
    constructor Create(TheOwner: TComponent); override;
    procedure AddWindow(const Window: TWindow);
    procedure RemoveWindow(const Window: TWindow);
    procedure RemoveWindow(const AHandle: HWND); overload;
    procedure UpdateWindow(const Window: TWindow);
    property Name: string read FName write FName;
    function IsEmpty: boolean;
  end;

  { TMenuItem }

  TMenuItem = class(Menus.TMenuItem)
  private
    procedure MenuItemClick(Sender: TObject);
  public
    constructor Create(TheOwner: TComponent); override;
  end;

  { TGroupedMenuItem }

  TGroupedMenuItem = class(Menus.TMenuItem)
  private
    FName: string;
    FRestoreGroupMenuItem: Menus.TMenuItem;
    procedure MenuItemClick(Sender: TObject);
    procedure RestoreWindowMenuItemClick(Sender: TObject);
    procedure RestoreGroupMenuItemClick(Sender: TObject);
  public
    constructor Create(TheOwner: TComponent); override;
    procedure AddWindow(const Window: TWindow);
    procedure RemoveWindow(const Window: TWindow);
    procedure RemoveWindow(const AHandle: HWND); overload;
    procedure UpdateWindow(const Window: TWindow);
    property Name: string read FName write FName;
    function IsEmpty: boolean;
  end;

  { TTrayManager }

  TTrayManager = class(TComponent, IFPObserver)
  type
    TTrayIconDictionary = specialize TObjectDictionary<HWND, TTrayIcon>;
    TMenuItemDictionary = specialize TObjectDictionary<HWND, TMenuItem>;
    TGroupedTrayIconDictionary = specialize TObjectDictionary<string, TGroupedTrayIcon>;
    TGroupedMenuItemDictionary = specialize TObjectDictionary<string, TGroupedMenuItem>;
  private
    FTrayIcons: TTrayIconDictionary;
    FMenuItems: TMenuItemDictionary;
    FGroupedTrayIcons: TGroupedTrayIconDictionary;
    FGroupedMenuItems: TGroupedMenuItemDictionary;
    FIconGrouped: boolean;
    FMenuGrouped: boolean;
    FMainMenu: TPopUpMenu;
    procedure SetIconGrouped(AValue: boolean);
    procedure SetMenuGrouped(AValue: boolean);
    procedure IconGroupedChanged(Sender: TObject);
    procedure MenuGroupedChanged(Sender: TObject);
    procedure AddWindow(const Window: TTrayWindow); 
    procedure UpdateWindow(const Window: TTrayWindow);
    procedure RemoveWindow(const Window: TTrayWindow);
    procedure DoRestoreWindow(Data: PtrInt);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure FPOObservedChanged(ASender: TObject; Operation: TFPObservedOperation; Data: Pointer);
    property IconGrouped: boolean read FIconGrouped write SetIconGrouped;
    property MenuGrouped: boolean read FMenuGrouped write SetMenuGrouped;
  end;

var
  TrayManager: TTrayManager = nil;
  MenuIconWidth, MenuIconHeight: integer;
  DefaultAppIcon: HICON;

implementation

uses
  Forms, LazFileUtils,
  Traynard.Types, Traynard.Settings, Traynard.Helpers, Traynard.Strings;

function GetIcon(Icon: HICON): HICON; inline;
begin
  if Icon = 0 then Exit(DefaultAppIcon);
  Result := Icon;
end;

{ TTrayIcon }

procedure TTrayIcon.TrayIconClick(Sender: TObject);
begin
  with Owner as TTrayManager do
    Application.QueueAsyncCall(@DoRestoreWindow, Self.Tag);
end;

constructor TTrayIcon.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  OnClick := @TrayIconClick;
end;

{ TGroupedTrayIcon }

procedure TGroupedTrayIcon.TrayIconClick(Sender: TObject);
var
  MenuItemCount: integer;
begin
  MenuItemCount := FPopUpMenu.Items.Count;
  if MenuItemCount = 3 then
  begin
    with Owner as TTrayManager do
      Application.QueueAsyncCall(@DoRestoreWindow, FPopUpMenu.Items[MenuItemCount - 1].Tag);
  end;
end;

procedure TGroupedTrayIcon.RestoreWindowMenuItemClick(Sender: TObject);
begin
  with Owner as TTrayManager do
    Application.QueueAsyncCall(@DoRestoreWindow, (Sender as Menus.TMenuItem).Tag);
end;

procedure TGroupedTrayIcon.RestoreGroupMenuItemClick(Sender: TObject);
var
  Tags: array of PtrInt;
  i: integer;
begin
  SetLength(Tags, FPopUpMenu.Items.Count);
  for i := 0 to Length(Tags) - 1 do
    Tags[i] := FPopUpMenu.Items[i].Tag;

  for i := 0 to Length(Tags) - 1 do
  begin
    if Tags[i] = 0 then Continue;
    with Owner as TTrayManager do
      Application.QueueAsyncCall(@DoRestoreWindow, Tags[i]);
  end;
end;

constructor TGroupedTrayIcon.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  OnClick := @TrayIconClick;
  FPopUpMenu := TPopUpMenu.Create(Self);
  FRestoreGroupMenuItem := Menus.TMenuItem.Create(FPopUpMenu);
  FRestoreGroupMenuItem.Caption := MENU_ITEM_RESTORE_GROUP;
  FRestoreGroupMenuItem.OnClick := @RestoreGroupMenuItemClick;
  FPopUpMenu.Items.Add(FRestoreGroupMenuItem);
  FPopUpMenu.Items.AddSeparator;
end;

procedure TGroupedTrayIcon.AddWindow(const Window: TWindow);
var
  MenuItem: Menus.TMenuItem;
begin
  MenuItem := Menus.TMenuItem.Create(FPopUpMenu);
  MenuItem.Caption := Window.Text;
  MenuItem.Hint := Window.Text;
  MenuItem.Bitmap.LoadFromHIcon(GetIcon(Window.Icon), MenuIconWidth, MenuIconHeight);
  MenuItem.Tag := PtrInt(Window.Handle);
  MenuItem.OnClick := @RestoreWindowMenuItemClick;
  FPopUpMenu.Items.Add(MenuItem);
  case FPopUpMenu.Items.Count of
    3:
    begin
      Icon.Handle := GetIcon(Window.Icon);
      Hint := Window.Text;
    end;
    4:
    begin
      PopUpMenu := FPopUpMenu;
      Icon.LoadFromExe(Window.AppPath);
      Hint := Name;
    end;
  end;
end;

procedure TGroupedTrayIcon.RemoveWindow(const Window: TWindow);
begin
  RemoveWindow(Window.Handle);
end;

procedure TGroupedTrayIcon.RemoveWindow(const AHandle: HWND);
var
  MenuItem: Menus.TMenuItem;
  Window: TWindow;
  MenuItemBeforeCount, MenuItemAfterCount: integer;
begin
  MenuItemBeforeCount := FPopUpMenu.Items.Count;
  for MenuItem in FPopUpMenu.Items do
  begin
    if MenuItem.Tag = AHandle then
    begin
      FPopUpMenu.Items.Remove(MenuItem);
      Break;
    end;
  end;
  MenuItemAfterCount := FPopUpMenu.Items.Count;
  if (MenuItemBeforeCount <> MenuItemAfterCount) and (MenuItemAfterCount = 3) then
  begin
    PopUpMenu := nil;
    MenuItem := FPopUpMenu.Items[2];
    if WindowManager.Tray.Find(MenuItem.Tag, Window) then
    begin
      Icon.Handle := GetIcon(Window.Icon);
      Hint := Window.Text;
    end;
  end;
end;

procedure TGroupedTrayIcon.UpdateWindow(const Window: TWindow);
var
  MenuItem: Menus.TMenuItem;
begin
  for MenuItem in FPopUpMenu.Items do
  begin
    if MenuItem.Tag = Window.Handle then
    begin
      MenuItem.Caption := Window.Text;
      MenuItem.Hint := Window.Text;
      MenuItem.Bitmap.LoadFromHIcon(GetIcon(Window.Icon), MenuIconWidth, MenuIconHeight);
      Break;
    end;
  end;
  if FPopUpMenu.Items.Count = 3 then
  begin
    Hint := Window.Text;
    Icon.Handle := GetIcon(Window.Icon);
  end;
end;

function TGroupedTrayIcon.IsEmpty: boolean;
begin
  Exit(FPopUpMenu.Items.Count = 2);
end;

{ TMenuItem }

procedure TMenuItem.MenuItemClick(Sender: TObject);
begin
  with Owner as TTrayManager do
    Application.QueueAsyncCall(@DoRestoreWindow, Self.Tag);
end;

constructor TMenuItem.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  OnClick := @MenuItemClick;
end;

{ TGroupedMenuItem }

procedure TGroupedMenuItem.MenuItemClick(Sender: TObject);
begin
  if (Count = 0) and (Tag <> 0) then
  begin
    with Owner as TTrayManager do
      Application.QueueAsyncCall(@DoRestoreWindow, Self.Tag);
  end;
end;

procedure TGroupedMenuItem.RestoreWindowMenuItemClick(Sender: TObject);
begin
  with Owner as TTrayManager do
    Application.QueueAsyncCall(@DoRestoreWindow, (Sender as Menus.TMenuItem).Tag);
end;

procedure TGroupedMenuItem.RestoreGroupMenuItemClick(Sender: TObject);
var
  Tags: array of PtrInt;
  i: integer;
begin
  SetLength(Tags, Count);
  for i := 0 to Length(Tags) - 1 do
    Tags[i] := Items[i].Tag;

  for i := 0 to Length(Tags) - 1 do
  begin
    if Tags[i] = 0 then Continue;
    with Owner as TTrayManager do
      Application.QueueAsyncCall(@DoRestoreWindow, Tags[i]);
  end;
end;

constructor TGroupedMenuItem.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  OnClick := @MenuItemClick;
  FRestoreGroupMenuItem := Menus.TMenuItem.Create(Self);
  FRestoreGroupMenuItem.Caption := MENU_ITEM_RESTORE_GROUP;
  FRestoreGroupMenuItem.OnClick := @RestoreGroupMenuItemClick;
end;

procedure TGroupedMenuItem.AddWindow(const Window: TWindow);
var
  MenuItem: Menus.TMenuItem;
begin
  if Count = 0 then
  begin
    if Tag = 0 then
    begin
      Caption := Window.Text;
      Hint := Window.Text;
      Bitmap.LoadFromHIcon(GetIcon(Window.Icon), MenuIconWidth, MenuIconHeight);
      Tag := PtrInt(Window.Handle);
      Exit;
    end
    else
    begin
      MenuItem := Menus.TMenuItem.Create(Self);
      MenuItem.Caption := Caption;
      MenuItem.Hint := Hint;
      MenuItem.Bitmap := Bitmap;
      MenuItem.Tag := Tag;
      MenuItem.OnClick := @RestoreWindowMenuItemClick;
      Add(FRestoreGroupMenuItem);
      AddSeparator;
      Add(MenuItem);
      Caption := Name;
      Hint := Name;
      Bitmap.LoadFromExe(Window.AppPath, MenuIconWidth, MenuIconHeight);
      Tag := 0;
    end;
  end;
  MenuItem := Menus.TMenuItem.Create(Self);
  MenuItem.Caption := Window.Text;
  MenuItem.Hint := Window.Text;
  MenuItem.Bitmap.LoadFromHIcon(GetIcon(Window.Icon), MenuIconWidth, MenuIconHeight);
  MenuItem.Tag := PtrInt(Window.Handle);
  MenuItem.OnClick := @RestoreWindowMenuItemClick;
  Add(MenuItem);
end;

procedure TGroupedMenuItem.RemoveWindow(const Window: TWindow);
begin
  RemoveWindow(Window.Handle);
end;

procedure TGroupedMenuItem.RemoveWindow(const AHandle: HWND);
var
  MenuItem: Menus.TMenuItem;
  Window: TWindow;
begin
  if (Count = 0) and (Tag = AHandle) then
  begin
    Tag := 0;
    Exit;
  end;

  for MenuItem in Self do
  begin
    if MenuItem.Tag = AHandle then
    begin
      Remove(MenuItem);
      Break;
    end;
  end;

  if Count = 3 then
  begin
    MenuItem := Items[2];
    if WindowManager.Tray.Find(MenuItem.Tag, Window) then
    begin
      Caption := Window.Text;
      Hint := Window.Text;
      Bitmap.LoadFromHIcon(GetIcon(Window.Icon), MenuIconWidth, MenuIconHeight);
      Tag := Window.Handle;
    end;
    Clear;
  end;
end;

procedure TGroupedMenuItem.UpdateWindow(const Window: TWindow);
var
  MenuItem: Menus.TMenuItem;
begin
  if (Count = 0) and (Tag = Window.Handle) then
  begin
    Caption := Window.Text;
    Hint := Window.Text;
    Bitmap.LoadFromHIcon(GetIcon(Window.Icon), MenuIconWidth, MenuIconHeight);
    Exit;
  end;

  for MenuItem in Self do
  begin
    MenuItem.Caption := Window.Text;
    MenuItem.Hint := Window.Text;
    MenuItem.Bitmap.LoadFromHIcon(GetIcon(Window.Icon), MenuIconWidth, MenuIconHeight);
    Break;
  end;
end;

function TGroupedMenuItem.IsEmpty: boolean;
begin
  Result := (Count = 0) and (Tag = 0);
end;

{ TTrayManager }

procedure TTrayManager.SetIconGrouped(AValue: boolean);
var
  GroupedTrayIcon: TGroupedTrayIcon;
  TrayIcon: TTrayIcon;
begin
  if FIconGrouped = AValue then Exit;
  FIconGrouped := AValue;

  for GroupedTrayIcon in FGroupedTrayIcons.Values do
    GroupedTrayIcon.Visible := AValue;

  for TrayIcon in FTrayIcons.Values do
    TrayIcon.Visible := not AValue;
end;

procedure TTrayManager.SetMenuGrouped(AValue: boolean);
var
  GroupedMenuItem: TGroupedMenuItem;
  MenuItem: TMenuItem;
begin
  if FMenuGrouped = AValue then Exit;
  FMenuGrouped := AValue;

  for GroupedMenuItem in FGroupedMenuItems.Values do
    GroupedMenuItem.Visible := AValue;

  for MenuItem in FMenuItems.Values do
    MenuItem.Visible := not AValue;
end;

procedure TTrayManager.IconGroupedChanged(Sender: TObject);
begin
  IconGrouped := (Sender as TSettings).IconGrouped;
end;

procedure TTrayManager.MenuGroupedChanged(Sender: TObject);
begin
  MenuGrouped := (Sender as TSettings).MenuGrouped;
end;

procedure TTrayManager.AddWindow(const Window: TTrayWindow);

  procedure AddIcon;
  var
    GroupedTrayIcon: TGroupedTrayIcon;
    TrayIcon: TTrayIcon;
  begin
    if not FGroupedTrayIcons.TryGetValue(Window.AppPath, GroupedTrayIcon) then
    begin
      GroupedTrayIcon := TGroupedTrayIcon.Create(Self);
      GroupedTrayIcon.Name := ExtractFileNameOnly(Window.AppPath);
      FGroupedTrayIcons.Add(Window.AppPath, GroupedTrayIcon);
    end;
    GroupedTrayIcon.AddWindow(Window);

    TrayIcon := TTrayIcon.Create(Self);
    TrayIcon.Icon.Handle := GetIcon(Window.Icon);
    TrayIcon.Hint := Window.Text;
    TrayIcon.Tag := Window.Handle;
    FTrayIcons.Add(Window.Handle, TrayIcon);

    if IconGrouped then
      GroupedTrayIcon.Visible := True
    else
      TrayIcon.Visible := True;
  end;

  procedure AddMenu;
  var
    GroupedMenuItem: TGroupedMenuItem;
    MenuItem: TMenuItem;
  begin
    if not FGroupedMenuItems.TryGetValue(Window.AppPath, GroupedMenuItem) then
    begin
      GroupedMenuItem := TGroupedMenuItem.Create(Self);
      GroupedMenuItem.Name := ExtractFileNameOnly(Window.AppPath);
      FGroupedMenuItems.Add(Window.AppPath, GroupedMenuItem);
      FMainMenu.Items.Insert(2, GroupedMenuItem);
    end;
    GroupedMenuItem.AddWindow(Window);

    MenuItem := TMenuItem.Create(Self);
    MenuItem.Bitmap.LoadFromHIcon(GetIcon(Window.Icon), MenuIconWidth, MenuIconHeight);
    MenuItem.Caption := Window.Text;
    MenuItem.Hint := Window.Text;
    MenuItem.Tag := Window.Handle;
    FMenuItems.Add(Window.Handle, MenuItem);
    FMainMenu.Items.Insert(2, MenuItem);

    if not MenuGrouped then
      GroupedMenuItem.Visible := False
    else
      MenuItem.Visible := False;
  end;

begin
  case Window.Position of
    tpIcon: AddIcon;
    tpMenu: AddMenu;
  end;
end;

procedure TTrayManager.UpdateWindow(const Window: TTrayWindow);

  procedure UpdateIcon;
  var
    TrayIcon: TTrayIcon;
    GroupedTrayIcon: TGroupedTrayIcon;
  begin
    if FGroupedTrayIcons.TryGetValue(Window.AppPath, GroupedTrayIcon) then
    begin
      GroupedTrayIcon.UpdateWindow(Window);
    end;

    if FTrayIcons.TryGetValue(Window.Handle, TrayIcon) then
    begin
      TrayIcon.Icon.Handle := GetIcon(Window.Icon);
      TrayIcon.Hint := Window.Text;
    end;
  end;

  procedure UpdateMenu;
  var
    MenuItem: TMenuItem;
    GroupedMenuItem: TGroupedMenuItem;
  begin
    if FGroupedMenuItems.TryGetValue(Window.AppPath, GroupedMenuItem) then
    begin
      GroupedMenuItem.UpdateWindow(Window);
    end;

    if FMenuItems.TryGetValue(Window.Handle, MenuItem) then
    begin
      MenuItem.Bitmap.LoadFromHIcon(GetIcon(Window.Icon), MenuIconWidth, MenuIconHeight);
      MenuItem.Caption := Window.Text;
      MenuItem.Hint := Window.Text;
    end;
  end;

begin
  case Window.Position of
    tpIcon: UpdateIcon;
    tpMenu: UpdateMenu;
  end;
end;

procedure TTrayManager.RemoveWindow(const Window: TTrayWindow);

  procedure RemoveIcon;
  var
    GroupedTrayIcon: TGroupedTrayIcon;
  begin
    FTrayIcons.Remove(Window.Handle);
    if FGroupedTrayIcons.TryGetValue(Window.AppPath, GroupedTrayIcon) then
    begin
      GroupedTrayIcon.RemoveWindow(Window);
      if GroupedTrayIcon.IsEmpty then FGroupedTrayIcons.Remove(Window.AppPath);
    end;
  end;

  procedure RemoveMenu;
  var
    GroupedMenuItem: TGroupedMenuItem;
  begin
    FMenuItems.Remove(Window.Handle);
    if FGroupedMenuItems.TryGetValue(Window.AppPath, GroupedMenuItem) then
    begin
      GroupedMenuItem.RemoveWindow(Window);
      if GroupedMenuItem.IsEmpty then FGroupedMenuItems.Remove(Window.AppPath);
    end;
  end;

begin
  case Window.Position of
    tpIcon: RemoveIcon;
    tpMenu: RemoveMenu;
  end;
end;

procedure TTrayManager.DoRestoreWindow(Data: PtrInt);
var
  Handle: HWND;
  GroupedTrayIcon: TGroupedTrayIcon;
  GroupedMenuItem: TGroupedMenuItem;
begin
  Handle := HWND(Data);
  try
    WindowManager.RestoreWindow(Handle);
  except
    Self.FMenuItems.Remove(Handle); 
    Self.FTrayIcons.Remove(Handle);
    for GroupedTrayIcon in FGroupedTrayIcons.Values do
      GroupedTrayIcon.RemoveWindow(Handle);
    for GroupedMenuItem in FGroupedMenuItems.Values do
      GroupedMenuItem.RemoveWindow(Handle);
  end;
end;

constructor TTrayManager.Create(AOwner: TComponent);
var
  Window: TWindow;
begin
  inherited Create(AOwner);

  FMainMenu := (AOwner as TForm).PopupMenu;
  FTrayIcons := TTrayIconDictionary.Create([doOwnsValues]); 
  FMenuItems := TMenuItemDictionary.Create([doOwnsValues]);
  FGroupedTrayIcons := TGroupedTrayIconDictionary.Create([doOwnsValues]);
  FGroupedMenuItems := TGroupedMenuItemDictionary.Create([doOwnsValues]);
  FIconGrouped := Settings.IconGrouped;
  FMenuGrouped := Settings.MenuGrouped;
  for Window in WindowManager.Tray do AddWindow(Window as TTrayWindow);
  Settings.AddListener(siIconGrouped, @IconGroupedChanged);
  Settings.AddListener(siMenuGrouped, @MenuGroupedChanged);
  WindowManager.Tray.FPOAttachObserver(Self);
end;

destructor TTrayManager.Destroy;
begin
  WindowManager.Tray.FPODetachObserver(Self);
  Settings.RemoveListeners(Self);
  FreeAndNil(FTrayIcons);
  FreeAndNil(FMenuItems);
  FreeAndNil(FGroupedTrayIcons);
  FreeAndNil(FGroupedMenuItems);
  inherited Destroy;
end;

procedure TTrayManager.FPOObservedChanged(ASender: TObject; Operation: TFPObservedOperation; Data: Pointer);
var
  Window: TWindow;
begin
  if not TTrayWindowCollection(ASender).Find(HWND(Data), Window) then Exit;

  case Operation of
    ooAddItem: AddWindow(Window as TTrayWindow);
    ooChange: UpdateWindow(Window as TTrayWindow);
    ooDeleteItem: RemoveWindow(Window as TTrayWindow);
  end;
end;

initialization

MenuIconWidth := GetSystemMetrics(SM_CXSMICON);
MenuIconHeight := GetSystemMetrics(SM_CYSMICON);
DefaultAppIcon := LoadIcon(0, IDI_APPLICATION);

end.

