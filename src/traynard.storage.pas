unit Traynard.Storage;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, TOML.Types, Traynard.Types;

type

  TConfig = TTOMLTable;

  TResourceType = (rtLanguage, rtLibrary);

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

  { TResource }

  TResource = record            
    ResourceType: TResourceType;
    ResourceName: string;
    FileName: string;
  end;

  { TStorage }

  TStorage = class
  type
    Exception = class(Sysutils.Exception);
  private
    FConfigured: boolean;
    FAppDataDir: string;
    FConfigDir: string;
    FLanguagesDir: string;
    procedure ExtractResource(constref Resource: TResource);
  public
    constructor Create;
    destructor Destroy; override;
    property AppDataDir: string read FAppDataDir;
    property ConfigDir: string read FConfigDir;
    property LanguagesDir: string read FLanguagesDir;
    property Configured: boolean read FConfigured;
    function LoadConfig(const AFilename: string; out Config: TConfig): boolean;
    function SaveConfig(const AFilename: string; const Config: TConfig): boolean;
    function OpenOrCreate(const AFilename: string; out Stream: TFileStream): boolean;
  end;

var
  Storage: TStorage = nil;

implementation

uses
  TOML, FileUtil, Windows, Traynard.Strings;

const
  CONFIG_EXT = '.toml';
  KEY_NOT_FOUND = 'Key not found';
  LOCAL_APP_DATA_DIR = 'data';
  CONFIG_DIR = 'config';
  LANGUAGES_DIR = 'languages';

{$IFDEF STANDALONE}
  BUILTIN_LANGUAGES: array of TResource = (
    (ResourceType: rtLanguage; ResourceName: 'LANG_ZH_CN'; FileName: 'zh_CN.mo'),
    (ResourceType: rtLanguage; ResourceName: 'LANG_ZH_TW'; FileName: 'zh_TW.mo'),
    (ResourceType: rtLanguage; ResourceName: 'LANG_ZH_HK'; FileName: 'zh_HK.mo'),
    (ResourceType: rtLibrary; ResourceName: 'LIB_HOOK'; FileName: HOOK_DLL)
  );
  {$R standalone.res}
{$ENDIF}

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

procedure TStorage.ExtractResource(constref Resource: TResource);
var
  ResourceStream: TResourceStream;
  FileStream: TFileStream;
  PathDir: string;
begin
  case Resource.ResourceType of
    rtLanguage: PathDir := LanguagesDir;
    rtLibrary:  PathDir := AppDataDir;
  else
    Exit;
  end;
  ResourceStream := TResourceStream.Create(HInstance, Resource.ResourceName, RT_RCDATA);
  try
    ForceDirectories(PathDir);
    FileStream := TFileStream.Create(PathDir + Resource.Filename, fmCreate);
    try
      FileStream.CopyFrom(ResourceStream, ResourceStream.Size);
    finally
      FileStream.Free;
    end;
  finally
    ResourceStream.Free;
  end;
end;

constructor TStorage.Create;
var
  AppDir: string;
  {$IFDEF STANDALONE}
  i: integer;
  {$ENDIF}
begin
  AppDir := ExtractFilePath(ParamStr(0));

  FAppDataDir := IncludeTrailingPathDelimiter(ConcatPaths([AppDir, LOCAL_APP_DATA_DIR]));
  if not DirectoryExists(FAppDataDir) then
    FAppDataDir := GetAppConfigDir(False);

  FConfigDir := IncludeTrailingPathDelimiter(FAppDataDir + CONFIG_DIR);
  FConfigured := DirectoryExists(FConfigDir);

  {$IFDEF STANDALONE}
  FLanguagesDir := IncludeTrailingPathDelimiter(FAppDataDir + LANGUAGES_DIR);
  if not FConfigured then
  begin
    try
      for i := Low(BUILTIN_LANGUAGES) to High(BUILTIN_LANGUAGES) do
        ExtractResource(BUILTIN_LANGUAGES[i]);
    except
    end;
  end;
  {$ELSE}
  FLanguagesDir := IncludeTrailingPathDelimiter(ConcatPaths([AppDir, LANGUAGES_DIR]));
  if not DirectoryExists(FLanguagesDir) then
    FLanguagesDir := IncludeTrailingPathDelimiter(FAppDataDir + LANGUAGES_DIR);
  {$ENDIF}
end;

destructor TStorage.Destroy;
begin
  inherited Destroy;
end;

function TStorage.LoadConfig(const AFilename: string; out Config: TConfig): boolean;
var
  FullPath: string;
begin
  Config := nil;

  if not FConfigured then
    ForceDirectories(FConfigDir);

  FullPath := FConfigDir + AFilename + CONFIG_EXT;
  try
    Config := ParseTOMLFromFile(FullPath);
    Exit(True);
  except
    on EFOpenError do Config := TOMLTable;
    on ETOMLException do
      raise Exception.CreateFmt(ERROR_PARSE_TOML_FILE, [FullPath]);
  end;
  Result := False;
end;

function TStorage.SaveConfig(const AFilename: string; const Config: TConfig): boolean;
begin
  Result := SerializeTOMLToFile(Config, FConfigDir + AFilename + CONFIG_EXT);
end;

function TStorage.OpenOrCreate(const AFilename: string; out Stream: TFileStream): boolean;
var
  FilePath: string;
begin
  Stream := nil;
  FilePath := FAppDataDir + AFilename;
  Result := FileExists(FilePath);
  try
    Stream := TFileStream.Create(FilePath, specialize IfThen<Word>(Result, fmOpenReadWrite, fmCreate) or fmShareExclusive);
  except
    on E: EStreamError do
      raise Exception.Create(E.Message);
  end;
end;

initialization

Storage := TStorage.Create;

finalization

FreeAndNil(Storage);

end.

