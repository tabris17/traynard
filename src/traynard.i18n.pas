unit Traynard.I18n;

{$mode ObjFPC}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, Generics.Collections, LCLTranslator, TypInfo;

type

  { TRestorableTranslator }

  TRestorableTranslator = class(TDefaultTranslator)
  type

    { TRestoreTranslator }

    TRestoreTranslator = class(TUpdateTranslator)
    private
      FTranslatedTerms: specialize TDictionary<string, string>;
    public
      constructor Create;
      destructor Destroy; override;
      property TranslatedTerms: specialize TDictionary<string, string> read FTranslatedTerms;
      procedure TranslateStringProperty(Sender: TObject; const Instance: TPersistent;
        PropInfo: PPropInfo; var Content: string); override;
    end;
  private
    FRestoreTranslator: TRestoreTranslator;
  public
    constructor Create(MOFileName: string);
    destructor Destroy; override;
    procedure TranslateStringProperty(Sender: TObject; const Instance: TPersistent;
      PropInfo: PPropInfo; var Content: string); override;
    procedure Restore;
  end;

  { TLanguage }

  TLanguage = record
    Code: string;
    Name: string;
    function ToString: string;
  end;

  TLanguageList = array of TLanguage;

  { TI18n }

  TI18n = class
  private
    FLanguageList: TLanguageList;
    FLocalLanguage: string;
    FTranslated: boolean;
    procedure ExtractLanguageResource(const ResourceName, Filename: string);
    procedure LanguageChanged(Sender: TObject);
    function GetAvailableLanguages: TLanguageList;
  public
    constructor Create;
    destructor Destroy; override;
    property AvailableLanguages: TLanguageList read GetAvailableLanguages;
    property LocalLanguage: string read FLocalLanguage;
    property Translated: boolean read FTranslated;
    procedure RefreshAvailableLanguages;
    procedure Translate(const Lang: string);
    procedure Translate; overload;
  end;

  { TLanguageResource }

  TLanguageResource = record
    ResourceName: string;
    FileName: string;
  end;

{$IFDEF ALLINONE}
const
  BUILTIN_LANGUAGES: array of TLanguageResource = (
    (ResourceName: 'ZH_CN_MO'; FileName: 'zh_CN.mo'), 
    (ResourceName: 'ZH_TW_MO'; FileName: 'zh_TW.mo'),
    (ResourceName: 'ZH_HK_MO'; FileName: 'zh_HK.mo')
  );
{$ENDIF}

var
  I18n: TI18n;

implementation

uses
  Forms, GetText, Windows, LazUTF8, LazFileUtils, LResources, Translations,
  Traynard.Storage, Traynard.Strings, Traynard.Settings, Traynard.Helpers, Traynard.Types;

function ParseLanguage(Header: string): TLanguage; inline;
var
  HeaderLines: TStringList;
  Line, Value, Key: string;
  KeyValuePair: TStringArray;
begin
  HeaderLines := TStringList.Create;
  try
    HeaderLines.Text := Header;
    for Line in HeaderLines do
    begin
      KeyValuePair := Line.Split(':', 2);
      Key := KeyValuePair[0].Trim;
      Value := KeyValuePair[1].Trim;
      case Key of
        'Language': Result.Code := Value;
        'X-Language-Name': Result.Name := Value;
      end;
    end;
  finally
    HeaderLines.Free;
  end;
end;

function DefaultTranslate(Name, Value: ansistring; Hash: longint; arg: pointer): ansistring;
begin
  Result := Value;
end;

{ TRestorableTranslator }

constructor TRestorableTranslator.Create(MOFileName: string);
begin
  inherited Create(MOFileName);
  FRestoreTranslator := TRestoreTranslator.Create;
end;

destructor TRestorableTranslator.Destroy;
begin
  FreeAndNil(FRestoreTranslator);
  inherited Destroy;
end;

procedure TRestorableTranslator.TranslateStringProperty(Sender: TObject; const Instance: TPersistent;
  PropInfo: PPropInfo; var Content: string);
var
  OriginalContent, NamePath: string;
begin
  OriginalContent := Content;
  inherited;
  if Content <> OriginalContent then
  begin
    NamePath := GetPersistentPath(Instance) + PropInfo^.Name;
    FRestoreTranslator.TranslatedTerms.TryAdd(NamePath + #4 + Content, OriginalContent);
  end;
end;

procedure TRestorableTranslator.Restore;
var
  i: integer;
  LocalTranslator: TAbstractTranslator;
begin
  LocalTranslator := LRSTranslator;
  LRSTranslator := FRestoreTranslator;
  for i := 0 to Screen.CustomFormCount-1 do
    FRestoreTranslator.UpdateTranslation(Screen.CustomForms[i]);
  for i := 0 to Screen.DataModuleCount-1 do
    FRestoreTranslator.UpdateTranslation(Screen.DataModules[i]);
  LRSTranslator := LocalTranslator;
end;

{ TRestorableTranslator.TRestoreTranslator }

constructor TRestorableTranslator.TRestoreTranslator.Create;
begin
  FTranslatedTerms := specialize TDictionary<string, string>.Create;
end;

destructor TRestorableTranslator.TRestoreTranslator.Destroy;
begin
  FreeAndNil(FTranslatedTerms);
  inherited Destroy;
end;

procedure TRestorableTranslator.TRestoreTranslator.TranslateStringProperty(Sender: TObject; const Instance: TPersistent;
  PropInfo: PPropInfo; var Content: string);
var
  Value, NamePath: string;
begin
  NamePath := GetPersistentPath(Instance) + PropInfo^.Name;
  if FTranslatedTerms.TryGetValue(NamePath + #4 + Content, Value) then
    Content := Value;
end;

{ TLanguage }

function TLanguage.ToString: string;
begin
  Result := specialize IfThen<string>(Code = '', Name, Format('%s (%s)', [Name, Code]));
end;

{ TI18n }

procedure TI18n.ExtractLanguageResource(const ResourceName, Filename: string);
var
  ResourceStream: TResourceStream;
  FileStream: TFileStream;
begin
  ResourceStream := TResourceStream.Create(HInstance, ResourceName, RT_RCDATA);
  try
    FileStream := TFileStream.Create(Storage.LanguagesDir + Filename, fmCreate);
    try
      FileStream.CopyFrom(ResourceStream, ResourceStream.Size);
    finally
      FileStream.Free;
    end;
  finally
    ResourceStream.Free;
  end;
end;

procedure TI18n.LanguageChanged(Sender: TObject);
begin
  Translate((Sender as TSettings).Language);
end;

constructor TI18n.Create;
var
  LangRes: TLanguageResource;
begin
  {$IFDEF ALLINONE}
  if Storage.FirstRun then
  begin
    for LangRes in BUILTIN_LANGUAGES do
      ExtractLanguageResource(LangRes.ResourceName, LangRes.FileName);
  end;
  {$ENDIF}
  FLanguageList := nil;
  FTranslated := False;
  FLocalLanguage := GetLanguageID.LanguageID;
  Settings.AddListener(siLanguage, @LanguageChanged);
end;

destructor TI18n.Destroy;
begin
  Settings.RemoveListeners(Self);
  inherited Destroy;
end;

procedure TI18n.RefreshAvailableLanguages;
begin
  FLanguageList := nil;
end;

function TI18n.GetAvailableLanguages: TLanguageList;
var
  SearchRec: TSearchRec;
  Count: integer;
  MOFile: TMOFile;
begin
  if Assigned(FLanguageList) then Exit(FLanguageList);

  Count := 2;
  SetLength(FLanguageList, Count);
  with FLanguageList[0] do
  begin
    Code := '';
    Name := TEXT_ANGLE_BRACKETED_AUTO;
  end;
  with FLanguageList[1] do
  begin
    Code := NATIVE_LANGUAGE_CODE;
    Name := NATIVE_LANGUAGE_NAME;
  end;
  if FindFirstUTF8(ConcatPaths([Storage.LanguagesDir, '*.mo']), faAnyFile, SearchRec) = 0 then
  begin
    repeat
      if (SearchRec.Name = '.') or (SearchRec.Name = '..') then
        Continue;
      SetLength(FLanguageList, Count + 1);
      try
        MOFile := TMOFile.Create(UTF8ToSys(ConcatPaths([Storage.LanguagesDir, SearchRec.Name])));
        try
          FLanguageList[Count] := ParseLanguage(MOFile.Translate('', 0, 0));
        finally
          MOFile.Free;
        end;
      except
        Inc(Count);
        Continue;
      end;
      Inc(Count);
    until FindNextUTF8(SearchRec) <> 0;
    FindCloseUTF8(SearchRec);
  end;

  Result := FLanguageList;
end;

procedure TI18n.Translate(const Lang: string);

  function SetLanguage(const MOFilename: string): boolean;
  var
    Translator: TRestorableTranslator;
    i: Integer;
    MOFile: TMOFile;
  begin
    try
      MOFile := TMOFile.Create(UTF8ToSys(MOFilename));
      try
        GetText.TranslateResourceStrings(MOFile);
      finally
        MOFile.Free;
      end;
      Translator := TRestorableTranslator.Create(MOFilename);
    except
      Exit(False);
    end;
    if Assigned(LRSTranslator) then
    begin
      if LRSTranslator is TRestorableTranslator then
        (LRSTranslator as TRestorableTranslator).Restore;
      LRSTranslator.Free;
    end;
    LRSTranslator := Translator;
    for i := 0 to Screen.CustomFormCount-1 do
      Translator.UpdateTranslation(Screen.CustomForms[i]);
    for i := 0 to Screen.DataModuleCount-1 do
      Translator.UpdateTranslation(Screen.DataModules[i]);

    FTranslated := True;
    Result := True;
  end;

var
  LangCode: string;
begin
  LangCode := Lang;
  if LangCode = '' then
    LangCode := FLocalLanguage;

  if (LangCode = NATIVE_LANGUAGE_CODE) or
     not SetLanguage(Storage.LanguagesDir + LangCode + '.mo') then
  begin
    SetResourceStrings(@DefaultTranslate, nil);
    if Assigned(LRSTranslator) then
    begin
      if LRSTranslator is TRestorableTranslator then
        (LRSTranslator as TRestorableTranslator).Restore;
      FreeAndNil(LRSTranslator);
    end;
    FTranslated := False;
  end;
end;

procedure TI18n.Translate;
begin
  Translate(Settings.Language);
end;

initialization

I18n := TI18n.Create;

finalization

FreeAndNil(I18n);

end.

