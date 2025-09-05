unit Traynard.Autorun;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TAutorunManager }

  TAutorunManager = class(TComponent)
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure AutorunChanged(Sender: TObject);
  end;

var
  AutorunManager: TAutorunManager = nil;

implementation

uses
  Windows, JwaWinReg, Traynard.Settings, Traynard.Types, Traynard.Helpers, Traynard.Strings;

{ TAutorunManager }

constructor TAutorunManager.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  Settings.Autorun := ERROR_SUCCESS = RegGetValueW(
    HKEY_CURRENT_USER,
    unicodestring(REG_AUTORUN),
    unicodestring(APP_NAME),
    RRF_RT_REG_SZ,
    nil, nil, nil
  );
  Settings.AddListener(siAutorun, @AutorunChanged);
end;

destructor TAutorunManager.Destroy;
begin
  Settings.RemoveListeners(Self);
  inherited Destroy;
end;

procedure TAutorunManager.AutorunChanged(Sender: TObject);
var
  RegKey: HKEY;
  CommandLine: unicodestring;
begin
  if ERROR_SUCCESS <> RegOpenKeyW(HKEY_CURRENT_USER, unicodestring(REG_AUTORUN), RegKey) then Exit;
  if (Sender as TSettings).Autorun then
  begin
    CommandLine := unicodestring(Format('"%s" -%s', [ParamStr(0), ARGUMENT_SILENT_CHAR]));
    RegSetValueExW(RegKey, unicodestring(APP_NAME), 0, REG_SZ, PByte(CommandLine), Length(CommandLine) * SizeOf(WideChar));
  end
  else
    RegDeleteValueW(RegKey, unicodestring(APP_NAME));
  RegCloseKey(RegKey);
end;

end.

