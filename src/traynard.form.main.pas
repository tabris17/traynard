unit Traynard.Form.Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ActnList,
  LMessages, Menus, ComCtrls, StdCtrls, Windows, Generics.Collections,
  Traynard.Strings, Traynard.Types, Traynard.Page, Traynard.Form.Popup;

type

  { TFormMain }

  TFormMain = class(TForm)
    ActionRules: TAction;
    ActionExit: TAction;
    ActionLicense: TAction;
    ActionTray: TAction;
    ActionOptions: TAction;
    ActionDesktop: TAction;
    ActionAbout: TAction;
    ActionList: TActionList;
    MenuItem1: TMenuItem;
    MenuItemLicense: TMenuItem;
    MenuItemTray: TMenuItem;
    MenuItemDesktop: TMenuItem;
    MenuItemExit: TMenuItem;
    MenuItemGitHub: TMenuItem;
    MenuItemIssue: TMenuItem;
    MenuItemUsage: TMenuItem;
    MenuItemOptions: TMenuItem;
    MinorIcons: TImageList;
    Separator2: TMenuItem;
    Separator3: TMenuItem;
    Separator4: TMenuItem;
    Separator5: TMenuItem;
    Spacer1: TLabel;
    MainLightIcons: TImageList;
    MainMenu: TPopupMenu;
    MenuItemAbout: TMenuItem;
    MenuItemHelp: TMenuItem;
    MainSolidIcons: TImageList;
    MainGreyIcons: TImageList;
    StatusBar: TStatusBar;
    ToolBar: TToolBar;
    ToolButtonRules: TToolButton;
    ToolButtonTray: TToolButton;
    ToolButtonOptions: TToolButton;
    ToolButtonDesktop: TToolButton;
    ToolButtonAbout: TToolButton;
    ToolButtonMenu: TToolButton;
    procedure ActionAboutExecute(Sender: TObject);
    procedure ActionExitExecute(Sender: TObject);
    procedure ActionDesktopExecute(Sender: TObject);
    procedure ActionLicenseExecute(Sender: TObject);
    procedure ActionOptionsExecute(Sender: TObject);
    procedure ActionRulesExecute(Sender: TObject);
    procedure ActionTrayExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShortCut(var Msg: TLMKey; var Handled: Boolean);
    procedure FormShow(Sender: TObject);
    procedure MainMenuPopup(Sender: TObject);
    procedure MenuItemGitHubClick(Sender: TObject);
    procedure MenuItemIssueClick(Sender: TObject);
    procedure MenuItemUsageClick(Sender: TObject);
    procedure ToolBarResize(Sender: TObject);
  type
    TPageIndex = (piNone, piDesktop, piTray, piOptions, piAbout, piLicense, piRules, piLogs);
    THistory = specialize TList<TPageIndex>;
    TPopupList = specialize TList<TFormPopup>;
  private
    FNavButtonsHeight: Integer;
    FPages: array[TPageIndex] of TPage;
    FHistory: THistory;
    FCurrentPageIndex: TPageIndex;
    FPageButtons: array[TPageIndex] of TToolButton;
    FPopupList: TPopupList;
    FClosed: boolean;
    class var FPageClasses: array[TPageIndex] of TPageClass;
    procedure SetCurrentPageIndex(AValue: TPageIndex);
  public               
    class procedure RegisterPage(PageIndex: TPageIndex; PageClass: TPageClass);
    function Popup(const Title, Message: string; const PopupType: TPopupType = ptNone; const Timeout: Integer = 0): TFormPopup;
    function Navigate(PageIndex: TPageIndex): TPage;
    function CurrentPage: TPage;
    function Backward: TPage;
    property CurrentPageIndex: TPageIndex read FCurrentPageIndex write SetCurrentPageIndex;
    property Closed: boolean read FClosed;
    procedure PopupDestroy(Sender: TObject);
    procedure PopupShow(Sender: TObject);
  end;

var
  FormMain: TFormMain;

implementation

uses LCLIntf, JwaWinUser, LazLogger, Traynard.Page.Desktop, Traynard.Page.Tray, Traynard.Page.Options, Traynard.Page.About,
     Traynard.Page.License, Traynard.Page.Rules, Traynard.Page.Logs, Traynard.Helpers, Traynard.Window, Traynard.Form.Background;

{$R *.lfm}

  { TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
var
  ToolButton: Pointer;
  PageClass: TPageClass;
  Page: TPage;
  PageIndex: TPageIndex;
begin
  FClosed := False;
  Caption := Application.Title;
  FCurrentPageIndex := piNone;
  FHistory := THistory.Create;
  FPopupList := TPopupList.Create;

  { Calculate the total height of the sidebar navigation buttons }
  FNavButtonsHeight := ToolBar.Indent * ToolBar.ButtonCount + ToolButtonMenu.Top;
  for ToolButton in ToolBar.ButtonList do Inc(FNavButtonsHeight, TToolButton(ToolButton).Height);

  { Create and initialize all pages }
  for PageIndex := Low(FPageClasses) to High(FPageClasses) do
  begin
    FPageButtons[PageIndex] := nil;
    PageClass := FPageClasses[PageIndex];
    if PageClass = nil then Continue;
    Page := PageClass.Create(Self);
    FPages[PageIndex] := Page;
    Page.Parent := Self;
    Page.Initialize;
  end;

  { Bind toolbar buttons and page index }
  FPageButtons[piDesktop] := ToolButtonDesktop;
  FPageButtons[piTray] := ToolButtonTray;
  FPageButtons[piOptions] := ToolButtonOptions;
  FPageButtons[piRules] := ToolButtonRules;
end;

procedure TFormMain.ActionDesktopExecute(Sender: TObject);
begin
  Navigate(piDesktop);
end;

procedure TFormMain.ActionLicenseExecute(Sender: TObject);
begin
  Navigate(piLicense);
end;

procedure TFormMain.ActionOptionsExecute(Sender: TObject);
begin
  Navigate(piOptions);
end;

procedure TFormMain.ActionRulesExecute(Sender: TObject);
begin
  Navigate(piRules);
end;

procedure TFormMain.ActionAboutExecute(Sender: TObject);
begin
  Navigate(piAbout);
end;

procedure TFormMain.ActionExitExecute(Sender: TObject);
begin
  FormBackground.Close;
end;

procedure TFormMain.ActionTrayExecute(Sender: TObject);
begin
  Navigate(piTray);
end;

procedure TFormMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  FClosed := True;
end;

procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  Page: TPage;
begin
  for Page in FPages do
  begin
    if not Assigned(Page) or not Assigned(Page.OnCloseQuery) then Continue;
    Page.OnCloseQuery(Sender, CanClose);
    if not CanClose then Break;
  end;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
var
  APopup: TFormPopup;
begin
  FreeAndNil(FHistory);
  for APopup in FPopupList do APopup.OnDestroy := nil;
  FreeAndNil(FPopupList);
  FormMain := nil;
end;

procedure TFormMain.FormHide(Sender: TObject);
begin
  CurrentPage.Visible := False;
end;

procedure TFormMain.FormShortCut(var Msg: TLMKey; var Handled: Boolean);
var
  Modifiers: TShiftState;
begin
  Modifiers := KeyDataToShiftState(Msg.KeyData);
  if (Msg.CharCode = VK_APPS) and
     (Modifiers = []) and
     (Msg.Msg = CN_KEYDOWN) then
  begin
    Handled := True;
    with ToolButtonMenu do
    begin
      Down := True;
      with ClientToScreen(Point.Create(Left, Top + Height)) do
        DropdownMenu.Popup(X, Y);
      Down := False;
    end;
  end;
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
  if FCurrentPageIndex = piNone then Navigate(piDesktop);
  CurrentPage.Visible := True;
end;

procedure TFormMain.MainMenuPopup(Sender: TObject);
var
  PopupPoint: TPoint;
begin
  PopupPoint := MainMenu.PopupPoint;
  Inc(PopupPoint.X, ToolButtonMenu.Width);
  Dec(PopupPoint.Y, ToolButtonMenu.Height);
  MainMenu.PopupPoint := PopupPoint;
end;

procedure TFormMain.MenuItemGitHubClick(Sender: TObject);
begin
  OpenURL(URL_GITHUB);
end;

procedure TFormMain.MenuItemIssueClick(Sender: TObject);
begin
  OpenURL(URL_NEW_ISSUE);
end;

procedure TFormMain.MenuItemUsageClick(Sender: TObject);
begin
  OpenURL(URL_USAGE);
end;

procedure TFormMain.ToolBarResize(Sender: TObject);
begin
  Spacer1.Height := ToolBar.Height - FNavButtonsHeight;
end;

function TFormMain.Popup(const Title, Message: string; const PopupType: TPopupType; const Timeout: Integer): TFormPopup;
begin
  Result := TFormPopup.Create(Self);
  FPopupList.Add(Result);
  if Timeout > 0 then Result.Timeout := Timeout;
  Result.PopupType := PopupType;
  Result.Title := Title;
  Result.Message := Message;
  Result.OnDestroy := @PopupDestroy;
  Result.OnShow := @PopupShow;
  Result.ShowOnTop;
end;

function TFormMain.Navigate(PageIndex: TPageIndex): TPage;
begin
  if FHistory.Contains(CurrentPageIndex) then FHistory.Remove(CurrentPageIndex);
  FHistory.Add(CurrentPageIndex);
  CurrentPageIndex := PageIndex;
  Result := CurrentPage;
end;

function TFormMain.CurrentPage: TPage;
begin
  if FCurrentPageIndex = piNone then Exit(nil);
  Result := FPages[FCurrentPageIndex];
end;

function TFormMain.Backward: TPage;
begin
  if FHistory.Count > 0 then
  begin
    CurrentPageIndex := FHistory.Last;
    FHistory.Remove(CurrentPageIndex);
  end;
  Result := CurrentPage;
end;

procedure TFormMain.PopupDestroy(Sender: TObject);
begin
  FPopupList.Remove(Sender as TFormPopup);
end;

procedure TFormMain.PopupShow(Sender: TObject);
const
  MARGIN = 4;
var
  ThePopup, APopup: TFormPopup;
begin
  ThePopup := Sender as TFormPopup;
  ThePopup.Top := MARGIN;
  ThePopup.Left := Width - ThePopup.Width - MARGIN;
  for APopup in FPopupList do
  begin
    if APopup = ThePopup then Continue;
    APopup.Top := APopup.Top + ThePopup.Height + MARGIN;
  end;
end;

procedure TFormMain.SetCurrentPageIndex(AValue: TPageIndex);
var
  Page: TPage;
  Button: TToolButton;
begin
  Page := CurrentPage;
  if Assigned(Page) then Page.Visible := False;
  FCurrentPageIndex := AValue;
  Page := CurrentPage;
  Page.Visible := True;
  ToolBar.Visible := not Page.FullFrame;
  Button := FPageButtons[AValue];
  if Button <> nil then Button.Down := True;
end;

class procedure TFormMain.RegisterPage(PageIndex: TPageIndex; PageClass: TPageClass);
begin
  FPageClasses[PageIndex] := PageClass;
end;

initialization

TFormMain.RegisterPage(piNone, nil);
TFormMain.RegisterPage(piDesktop, TPageDesktop);
TFormMain.RegisterPage(piTray, TPageTray);
TFormMain.RegisterPage(piOptions, TPageOptions);
TFormMain.RegisterPage(piAbout, TPageAbout);  
TFormMain.RegisterPage(piLicense, TPageLicense);
TFormMain.RegisterPage(piRules, TPageRules);
TFormMain.RegisterPage(piLogs, TPageLogs);

end.

