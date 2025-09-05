unit Traynard.Page;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls, StdCtrls;

type

  { TFramePage }

  TFramePage = class(TFrame)
    HeaderTitle: TLabel;
    Header: TToolBar;
    HeaderMenu: TToolButton;
    HeaderBackward: TToolButton;
    procedure HeaderResize(Sender: TObject);
    procedure HeaderBackwardClick(Sender: TObject);
  private
    FTitle: TCaption;
    FShowStatusBar: Boolean;
    FOnPageActivate: TNotifyEvent;
    FOnPageDeactivate: TNotifyEvent;
    FOnCloseQuery: TCloseQueryEvent;
  protected
    procedure SetTitle(AValue: TCaption); virtual;
    procedure BackwardQuery(var CanBackward: Boolean); virtual;
    procedure DoPageActivate; virtual;
    procedure DoPageDeactivate; virtual;
    procedure VisibleChanged; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Initialize; virtual;
    procedure Finalize; virtual;
    function FullFrame: Boolean;
    property Title: TCaption read FTitle write SetTitle;
    property OnPageActivate: TNotifyEvent read FOnPageActivate write FOnPageActivate;
    property OnPageDeactivate: TNotifyEvent read FOnPageDeactivate write FOnPageDeactivate;
    property OnCloseQuery: TCloseQueryEvent read FOnCloseQuery write FOnCloseQuery ;
  end;

  TPage = TFramePage;

  TPageClass = class of TPage;

implementation

{$R *.lfm}

uses Traynard.Form.Main;

{ TFramePage }

procedure TFramePage.HeaderResize(Sender: TObject);
var
  LabelWidth: Integer;
  ToolButton: Pointer;
begin
  LabelWidth := Header.Width - Header.Indent * Header.ButtonCount;
  for ToolButton in Header.ButtonList do
    Dec(LabelWidth, TToolButton(ToolButton).Width);
  HeaderTitle.Width := LabelWidth;
end;

procedure TFramePage.HeaderBackwardClick(Sender: TObject);
var
  CanBackward: Boolean;
begin
  CanBackward := True;
  BackwardQuery(CanBackward);
  if CanBackward then TFormMain(Parent).Backward;
end;

function TFramePage.FullFrame: Boolean;
begin
  Result := Header.Visible;
end;

procedure TFramePage.SetTitle(AValue: TCaption);
begin
  if FTitle = AValue then Exit;
  FTitle := AValue;
  HeaderTitle.Caption := AValue;
end;

procedure TFramePage.BackwardQuery(var CanBackward: Boolean);
begin
  { not implemented }
end;

procedure TFramePage.DoPageActivate;
begin
  if Assigned(FOnPageActivate) then FOnPageActivate(Self);
end;

procedure TFramePage.DoPageDeactivate;
begin
  if Assigned(FOnPageDeactivate) then FOnPageDeactivate(Self);
end;

procedure TFramePage.VisibleChanged;
begin
  inherited VisibleChanged;
  if Visible then
    DoPageActivate
  else
    DoPageDeactivate;
end;

constructor TFramePage.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Align := alClient;
  Visible := False;
  FShowStatusBar := False;
  FOnPageActivate := nil;
  FOnPageDeactivate := nil;
  FOnCloseQuery := nil;
end;

destructor TFramePage.Destroy;
begin              
  if Visible then DoPageDeactivate;
  Finalize;
  inherited Destroy;
end;

procedure TFramePage.Initialize;
begin
  { not implemented }
end;

procedure TFramePage.Finalize;
begin
  { not implemented }
end;

end.

