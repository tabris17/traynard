unit Traynard.Storage;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, TOML.Types;

type

  TConfig = TTOMLTable;

  { TConfigHelper }

  TConfigHelper = class helper for TConfig
    function GetValue(const FullKey: string): TTOMLValue;
    function GetString(const AKey: string; const Default: string = ''): string;
    function GetBoolean(const AKey: string; const Default: boolean = False): boolean;
    function GetInteger(const AKey: string; const Default: integer = 0): integer;
    function GetIntegerArrayItem(const AKey: string; const Index: integer; const Default: Integer = 0): integer;
    function Next(const AKey: string): TConfig;
    procedure SetValue(const FullKey: string; const AValue: TTOMLValue);
    procedure SetString(const AKey: string; const AValue: string);
    procedure SetBoolean(const AKey: string; const AValue: boolean);
    procedure SetInteger(const AKey: string; const AValue: integer);
    procedure SetIntegerArrayItem(const AKey: string; const Index: integer; const AValue: integer; const PaddingValue: integer = 0);
  end;

  { TStorage }

  TStorage = class
  private
    FConfigDir: string;
    FLanguagesDir: string;
  public
    constructor Create;
    destructor Destroy; override;
    property ConfigDir: string read FConfigDir;
    property LanguagesDir: string read FLanguagesDir;
    function Load(const AFilename: string; out Config: TConfig): boolean;
    function Save(const AFilename: string; const Config: TConfig): boolean;
  end;

var
  Storage: TStorage = nil;

implementation

uses
  TOML, FileUtil;

const
  EXT_NAME = '.toml';
  KEY_NOT_FOUND = 'Key not found';
  APP_DATA_DIR = 'data';
  LANGUAGES_DIR = 'languages';

{ TConfigHelper }

function TConfigHelper.GetValue(const FullKey: string): TTOMLValue;
var
  Key: string;
  Table: TTOMLTable;
begin
  Result := Self;
  for Key in FullKey.Split('.') do
  begin
    Table := Result as TTOMLTable;
    if not Table.TryGetValue(Key, Result) then
      raise ETOMLException.Create(KEY_NOT_FOUND);
  end;
end;

function TConfigHelper.GetString(const AKey: string; const Default: string): string;
begin
  try
    Result := GetValue(AKey).AsString;
  except
    Result := Default;
  end;
end;

function TConfigHelper.GetBoolean(const AKey: string; const Default: boolean): boolean;
begin
  try
    Result := GetValue(AKey).AsBoolean;
  except
    Result := Default;
  end;
end;

function TConfigHelper.GetInteger(const AKey: string; const Default: integer): integer;
begin
  try
    Result := GetValue(AKey).AsInteger;
  except
    Result := Default;
  end;
end;

function TConfigHelper.GetIntegerArrayItem(const AKey: string; const Index: integer; const Default: Integer): integer;
var
  ArrayValue: TTOMLArray;
begin
  try
    ArrayValue := GetValue(AKey).AsArray;
    Result := ArrayValue.Items[Index].AsInteger;
  except
    Result := Default;
  end;
end;

function TConfigHelper.Next(const AKey: string): TConfig;
var
  NextValue: TTOMLValue;
begin
  if TryGetValue(AKey, NextValue) then
  begin
    if NextValue is TTOMLTable then
      Exit(NextValue as TTOMLTable);
    Items.Remove(AKey);
  end;
  Result := TOMLTable;
  Add(AKey, Result);
end;

procedure TConfigHelper.SetValue(const FullKey: string; const AValue: TTOMLValue);
var
  Key: string;
  Keys: TStringArray;
  i, j: Integer;
  Config: TConfig;
  TheValue: TTOMLValue;
begin
  Keys := FullKey.Split('.');
  Config := Self;
  j := High(Keys);
  Key := Keys[j];

  for i := 0 to j - 1 do
    Config := Config.Next(Keys[i]);

  if Config.TryGetValue(Key, TheValue) then
  begin
    Config.Items.Remove(Key);
    TheValue.Free;
  end;
  Config.Add(Key, AValue);
end;

procedure TConfigHelper.SetString(const AKey: string; const AValue: string);
begin
  SetValue(AKey, TOMLString(AValue));
end;

procedure TConfigHelper.SetBoolean(const AKey: string; const AValue: boolean);
begin
  SetValue(AKey, TOMLBoolean(AValue));
end;

procedure TConfigHelper.SetInteger(const AKey: string; const AValue: integer);
begin
  SetValue(AKey, TOMLInteger(AValue));
end;

procedure TConfigHelper.SetIntegerArrayItem(const AKey: string; const Index: integer; const AValue: integer; const PaddingValue: integer);
var
  ArrayValue: TTOMLArray;
  ArraySize, i: integer;
begin
  try
    ArrayValue := GetValue(AKey).AsArray;
  except
    ArrayValue := TOMLArray;
    SetValue(AKey, ArrayValue);
  end;

  ArraySize := ArrayValue.Count;
  for i := ArraySize to Index do ArrayValue.Add(TOMLInteger(PaddingValue));
  ArrayValue.Items[Index].Free;
  ArrayValue.Items[Index] := TOMLInteger(AValue);
end;

{ TStorage }

constructor TStorage.Create;
begin
  FConfigDir := IncludeTrailingPathDelimiter(ConcatPaths([ExtractFilePath(ParamStr(0)), APP_DATA_DIR]));
  if not DirectoryExists(FConfigDir) then
  begin
    FConfigDir := GetAppConfigDir(False);
    ForceDirectories(FConfigDir);
  end;
  FLanguagesDir := IncludeTrailingPathDelimiter(FConfigDir + LANGUAGES_DIR);
  ForceDirectories(FLanguagesDir);
end;

destructor TStorage.Destroy;
begin
  inherited Destroy;
end;

function TStorage.Load(const AFilename: string; out Config: TConfig): boolean;
begin
  try
    Config := ParseTOMLFromFile(FConfigDir + AFilename + EXT_NAME);
    Exit(True);
  except
    on EFOpenError do Config := TOMLTable;
  end;
  Result := False;
end;

function TStorage.Save(const AFilename: string; const Config: TConfig): boolean;
begin
  Result := SerializeTOMLToFile(Config, FConfigDir + AFilename + EXT_NAME);
end;

initialization

Storage := TStorage.Create;

finalization

FreeAndNil(Storage);

end.

