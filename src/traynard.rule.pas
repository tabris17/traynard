unit Traynard.Rule;

{$mode ObjFPC}{$H+}
{$MODESWITCH ADVANCEDRECORDS}

interface

uses
  Classes, SysUtils, RegExpr, Generics.Collections,
  Traynard.Types, Traynard.Window, Traynard.Storage, TOML.Types;

type

  { TRule }

  TRule = record
    Name: string;
    WindowTitle: TRuleText;
    WindowClass: TRuleText;
    AppPath: TRuleText;
    TriggerOn: TRuleTriggerOn;
    Notification: TRuleNotification;
    Position: TTrayPosition;
    Hotkey: THotkey;

    procedure Validate;
    procedure Load(const Config: TConfig);
    procedure Save(const Config: TConfig);
  end;

  { TRules }

  TRules = class
  type
    TRuleList = specialize TList<string>;
    TRuleMap = specialize TDictionary<string, TRule>;

    { THotkeyRules }

    THotkeyRules = class(TRuleList)
    private
      FHotkeyID: longint;
    public
      constructor Create(AHotkeyID: longint);
      property HotkeyID: longint read FHotkeyID write FHotkeyID;
    end;

    THotkeyMap = specialize TObjectDictionary<THotkey, THotkeyRules>;
    THotkeyPair = specialize TPair<THotkey, THotkeyRules>;

    { TEnumerator }

    TEnumerator = class
    private
      FIndex: SizeInt;
      FList: TRuleList;
      FMap: TRuleMap;
      FOnDestroy: TNotifyEvent;
    protected
      function GetCurrent: TRule;
    public
      constructor Create(RuleList: TRuleList; RuleMap: TRuleMap);
      destructor Destroy; override;
      property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
      property Current: TRule read GetCurrent;
      function MoveNext: boolean;
    end;
  private
    FRuleList: TRuleList;
    FRuleMap: TRuleMap;
    FRegEx: TRegExpr;
    FConfig: TConfig;
    FConfigRules: TTOMLArray;
    FHotkeyMap: THotkeyMap;
    FHotkeyFilter: THotkey;
    FOnHotkeyAddedNotify: THotkeyAddedNotify;
    FOnHotkeyRemovedNotify: THotkeyRemovedNotify;
    function GetCount: SizeInt;
    function GetRule(Index: SizeInt): TRule;
    procedure RuleAdded(constref Rule: TRule); inline;
    procedure RuleRemoved(constref Rule: TRule); inline;
    procedure RuleUpdated(constref OldRule, NewRule: TRule); inline;
  protected
    procedure ResetFilter(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    property Rules[Index: SizeInt]: TRule read GetRule; default;
    property Count: SizeInt read GetCount;
    property Hotkeys: THotkeyMap read FHotkeyMap;
    property OnHotkeyAddedNotify: THotkeyAddedNotify read FOnHotkeyAddedNotify write FOnHotkeyAddedNotify;
    property OnHotkeyRemovedNotify: THotkeyRemovedNotify read FOnHotkeyRemovedNotify write FOnHotkeyRemovedNotify;
    function Find(const Window: TWindow; out Rule: TRule; const WindowAction: TWindowAction): boolean;
    function Match(const Window: TWindow; constref Rule: TRule): boolean;
    function AddRule(constref Rule: TRule): SizeInt;
    function GetEnumerator: TEnumerator;
    function HasRule(const Name: string; out Index: SizeInt): boolean;
    function Filter(const Hotkey: THotkey): TRules;
    procedure Load;
    procedure RemoveRule(const RuleIndex: SizeInt);
    procedure UpdateRule(const RuleIndex: SizeInt; constref Rule: TRule);
  end;

var
  Rules: TRules = nil;

const
  CONFIG_NAME = 'rules';
  ERROR_INVALID_RULE = 'Invalid rule';

  KEY_RULES = CONFIG_NAME;
  KEY_NAME = 'name';
  KEY_WINDOW_TITLE = 'window_title';
  KEY_WINDOW_CLASS = 'window_class';
  KEY_APP_PATH = 'app_path';        
  KEY_TRIGGER_ON = 'trigger_on';
  KEY_NOTIFICATION = 'notification';
  KEY_POSITION = 'position';
  KEY_HOTKEY = 'hotkey';
  KEY_SCHEMA = 'schema';

  RULE_SCHEMA_DEFAULT = 0;
  RULE_SCHEMA_V1 = 1;

implementation

uses
  LazLogger, TOML;

function Equal(constref RegEx: TRegExpr; const RuleText: TRuleText; const Text: string): boolean; inline;
begin
  case RuleText.Comparison of
    rtcAny:        Exit(True);
    rtcEquals:     Exit(RuleText.Text = Text);
    rtcContains:   Exit(Pos(RuleText.Text, Text) > 0);
    rtcStartsWith: Exit(Text.StartsWith(RuleText.Text));
    rtcEndsWith:   Exit(Text.EndsWith(RuleText.Text));
    rtcRegexMatch:
    begin
      RegEx.Expression := RuleText.Text;
      RegEx.InputString := Text;
      Exit(RegEx.Exec);
    end;
  end;
  Result := False;
end;

{ TRule }

procedure TRule.Validate;
var
  WindowActions: TRuleTriggerOn;
begin
  WindowActions := [waCreation, waChange];
  if TriggerOn * WindowActions <> [] then
    Include(TriggerOn, waExisting);

  if (Name = '') or
     (WindowTitle.Text = '') and (WindowTitle.Comparison <> rtcAny) or
     (WindowClass.Text = '') and (WindowClass.Comparison <> rtcAny) or
     (AppPath.Text = '') and (AppPath.Comparison <> rtcAny) or
     (TriggerOn = [])
  then
    raise Exception.Create(ERROR_INVALID_RULE);
end;

procedure TRule.Load(const Config: TConfig);
var
  Value: TTOMLValue;
  Schema: integer = RULE_SCHEMA_DEFAULT;
begin
  Name := Config.Items[KEY_NAME].AsString;

  if Config.TryGetValue(KEY_WINDOW_TITLE, Value) then
  begin
    with Value as TTOMLArray do
    begin
      WindowTitle.Text := Items[0].AsString;
      WindowTitle.Comparison := TRuleTextComparison(Items[1].AsInteger);
    end;
  end;

  if Config.TryGetValue(KEY_WINDOW_CLASS, Value) then
  begin
    with Value as TTOMLArray do
    begin
      WindowClass.Text := Items[0].AsString;
      WindowClass.Comparison := TRuleTextComparison(Items[1].AsInteger);
    end;
  end;

  if Config.TryGetValue(KEY_APP_PATH, Value) then
  begin
    with Value as TTOMLArray do
    begin
      AppPath.Text := Items[0].AsString;
      AppPath.Comparison := TRuleTextComparison(Items[1].AsInteger);
    end;
  end;

  TriggerOn := [];
  for Value in Config.Items[KEY_TRIGGER_ON].AsArray.Items do
  begin
    Include(TriggerOn, TWindowAction(Value.AsInteger));
  end;

  Notification := TRuleNotification(Config.Items[KEY_NOTIFICATION].AsInteger);
  Position := TTrayPosition(Config.Items[KEY_POSITION].AsInteger);
  Hotkey.Value := Config.GetInteger(KEY_HOTKEY, 0);

  if Config.TryGetValue(KEY_SCHEMA, Value) then Schema := Value.AsInteger;
  if Schema < RULE_SCHEMA_V1 then
  begin
    { fix `waExisting` issue }
    Exclude(TriggerOn, TWindowAction(4));
  end;

  Validate;
end;

procedure TRule.Save(const Config: TConfig);
var
  Value: TTOMLArray;
  WindowAction: TWindowAction;
begin
  Config.Add(KEY_NAME, TOMLString(Name));

  Value := TOMLArray;
  Value.Add(TOMLString(WindowTitle.Text));
  Value.Add(TOMLInteger(Ord(WindowTitle.Comparison)));
  Config.Add(KEY_WINDOW_TITLE, Value);

  Value := TOMLArray;
  Value.Add(TOMLString(WindowClass.Text));
  Value.Add(TOMLInteger(Ord(WindowClass.Comparison)));
  Config.Add(KEY_WINDOW_CLASS, Value);

  Value := TOMLArray;
  Value.Add(TOMLString(AppPath.Text));
  Value.Add(TOMLInteger(Ord(AppPath.Comparison)));
  Config.Add(KEY_APP_PATH, Value);

  Value := TOMLArray;
  for WindowAction := RULE_TRIGGER_ON_BEGIN to RULE_TRIGGER_ON_END do
  begin
    if WindowAction in TriggerOn then
      Value.Add(TOMLInteger(Ord(WindowAction)));
  end;
  Config.Add(KEY_TRIGGER_ON, Value);

  Config.Add(KEY_NOTIFICATION, TOMLInteger(Ord(Notification)));
  Config.Add(KEY_POSITION, TOMLInteger(Ord(Position)));
  Config.Add(KEY_HOTKEY, TOMLInteger(Hotkey.Value));
  Config.Add(KEY_SCHEMA, TOMLInteger(RULE_SCHEMA_V1));
end;

{ TRules }

function TRules.GetRule(Index: SizeInt): TRule;
begin
  Result := FRuleMap[FRuleList[Index]];
end;

procedure TRules.RuleAdded(constref Rule: TRule);
var
  RuleList: THotkeyRules;
  HotkeyID: longint = HOTKEY_NONE;
begin
  if not (waHotkey in Rule.TriggerOn) or (Rule.Hotkey.Value = HOTKEY_NONE) then Exit;
  if not FHotkeyMap.TryGetValue(Rule.Hotkey, RuleList) then
  begin
    if Assigned(FOnHotkeyAddedNotify) then FOnHotkeyAddedNotify(Rule.Hotkey, HotkeyID);
    RuleList := THotkeyRules.Create(HotkeyID);
    FHotkeyMap.Add(Rule.Hotkey, RuleList);
  end;
  RuleList.Add(Rule.Name);
end;

procedure TRules.RuleRemoved(constref Rule: TRule);
var
  RuleList: THotkeyRules;
begin
  if not (waHotkey in Rule.TriggerOn) or (Rule.Hotkey.Value = HOTKEY_NONE) or
     not FHotkeyMap.TryGetValue(Rule.Hotkey, RuleList) then Exit;
  if (RuleList.Remove(Rule.Name) >= 0) and (RuleList.Count = 0) then
  begin
    if Assigned(FOnHotkeyRemovedNotify) then FOnHotkeyRemovedNotify(RuleList.HotkeyID);
    FHotkeyMap.Remove(Rule.Hotkey);
  end;
end;

procedure TRules.RuleUpdated(constref OldRule, NewRule: TRule);
begin
  RuleRemoved(OldRule);
  RuleAdded(NewRule);
end;

procedure TRules.ResetFilter(Sender: TObject);
begin
  FHotkeyFilter.Value := HOTKEY_NONE;
end;

function TRules.GetEnumerator: TEnumerator;
var
  RuleList: THotkeyRules;
begin
  if FHotkeyFilter.Value = HOTKEY_NONE then
    Result := TEnumerator.Create(FRuleList, FRuleMap)
  else if FHotkeyMap.TryGetValue(FHotkeyFilter, RuleList) then
  begin
    Result := TEnumerator.Create(RuleList, FRuleMap);
    Result.OnDestroy := @ResetFilter;
  end
  else
    Result := TEnumerator.Create(nil, nil);
end;

function TRules.HasRule(const Name: string; out Index: SizeInt): boolean;
begin
  Result := FRuleMap.ContainsKey(Name);
  if Result then
    Index := FRuleList.IndexOf(Name);
end;

function TRules.Filter(const Hotkey: THotkey): TRules;
begin
  FHotkeyFilter := Hotkey;
  Result := Self;
end;

function TRules.GetCount: SizeInt;
begin
  Result := FRuleList.Count;
end;

constructor TRules.Create;
begin
  FOnHotkeyAddedNotify := nil;
  FOnHotkeyRemovedNotify := nil;
  FHotkeyFilter.Value := HOTKEY_NONE;
  FRuleList := TRuleList.Create;
  FRuleMap := TRuleMap.Create;
  FHotkeyMap := THotkeyMap.Create([doOwnsValues]);
  FRegEx := TRegExpr.Create;
end;

destructor TRules.Destroy;
begin
  if Assigned(FConfig) then
  begin
    Storage.SaveConfig(CONFIG_NAME, FConfig);
    FreeAndNil(FConfig);
  end;

  FreeAndNil(FRuleList);
  FreeAndNil(FRuleMap);
  FreeAndNil(FHotkeyMap);
  FreeAndNil(FRegEx);

  inherited Destroy;
end;

function TRules.Find(const Window: TWindow; out Rule: TRule; const WindowAction: TWindowAction): boolean;
begin
  for Rule in FRuleMap.Values do
  begin
    if (WindowAction in Rule.TriggerOn) and
       Equal(FRegEx, Rule.WindowTitle, Window.Text) and
       Equal(FRegEx, Rule.WindowClass, Window.ClassName) and
       Equal(FRegEx, Rule.AppPath, Window.AppPath) then Exit(True);
  end;
  Result := False;
end;

function TRules.Match(const Window: TWindow; constref Rule: TRule): boolean;
begin
  Result := Equal(FRegEx, Rule.WindowTitle, Window.Text) and
            Equal(FRegEx, Rule.WindowClass, Window.ClassName) and
            Equal(FRegEx, Rule.AppPath, Window.AppPath);
end;

function TRules.AddRule(constref Rule: TRule): SizeInt;
var
  ConfigRule: TConfig;
begin
  Rule.Validate;
  FRuleMap.Add(Rule.Name, Rule);
  Result := FRuleList.Add(Rule.Name);
  RuleAdded(Rule);

  ConfigRule := TOMLTable;
  Rule.Save(ConfigRule);
  FConfigRules.Add(ConfigRule);

  Storage.SaveConfig(CONFIG_NAME, FConfig);
end;

procedure TRules.Load;
var
  ConfigRules, ConfigRule: TTOMLValue;
  Rule: TRule;
begin
  if Assigned(FConfig) then
    raise Exception.Create('Duplicate rules loading');

  Storage.LoadConfig(CONFIG_NAME, FConfig);
  if FConfig.TryGetValue(KEY_RULES, ConfigRules) then
  begin
    if ConfigRules is TTOMLArray then
    begin
      FConfigRules := ConfigRules as TTOMLArray;
      for ConfigRule in FConfigRules.Items do
      begin
        try
          Rule.Load(ConfigRule as TTOMLTable);
        except
          Continue;
        end;
        if FRuleMap.TryAdd(Rule.Name, Rule) then
        begin
          FRuleList.Add(Rule.Name);
          RuleAdded(Rule);
        end;
      end;
    end
    else
    begin
      FConfig.Items.Remove(KEY_RULES);
      ConfigRules.Free;
      FConfigRules := TOMLArray;
      FConfig.Add(KEY_RULES, FConfigRules);
    end;
  end
  else
  begin
    FConfigRules := TOMLArray;
    FConfig.Add(KEY_RULES, FConfigRules);
  end;
end;

procedure TRules.RemoveRule(const RuleIndex: SizeInt);
var
  RuleName: string;
  Rule: TRule;
  ConfigRule: TTOMLValue;
begin
  RuleName := FRuleList[RuleIndex];
  Rule := FRuleMap[RuleName];
  FRuleList.Delete(RuleIndex);
  FRuleMap.Remove(RuleName);
  RuleRemoved(Rule);

  ConfigRule := FConfigRules.Items[RuleIndex];
  FConfigRules.Items.Remove(ConfigRule);
  ConfigRule.Free;

  Storage.SaveConfig(CONFIG_NAME, FConfig);
end;

procedure TRules.UpdateRule(const RuleIndex: SizeInt; constref Rule: TRule);
var
  OldConfigRule: TTOMLValue;
  NewConfigRule: TConfig;
  OldRule: TRule;
  OldRuleName: string;
begin
  OldRuleName := FRuleList[RuleIndex];
  OldRule := FRuleMap[OldRuleName];
  Rule.Validate;
  if OldRuleName = Rule.Name then
  begin
    FRuleMap[Rule.Name] := Rule;
  end
  else
  begin
    FRuleMap.Remove(OldRuleName);
    FRuleMap.Add(Rule.Name, Rule);
    FRuleList[RuleIndex] := Rule.Name;
  end;
  RuleUpdated(OldRule, Rule);

  OldConfigRule := FConfigRules.Items[RuleIndex];
  FConfigRules.Items.Remove(OldConfigRule);
  OldConfigRule.Free;
  NewConfigRule := TOMLTable;
  Rule.Save(NewConfigRule);
  FConfigRules.Items.Insert(RuleIndex, NewConfigRule);

  Storage.SaveConfig(CONFIG_NAME, FConfig);
end;

{ TRules.THotkeyRules }

constructor TRules.THotkeyRules.Create(AHotkeyID: longint);
begin
  inherited Create;
  FHotkeyID := AHotkeyID;
end;

{ TRules.TEnumerator }

function TRules.TEnumerator.GetCurrent: TRule;
begin
  Result := FMap[FList[FIndex]];
end;

constructor TRules.TEnumerator.Create(RuleList: TRuleList; RuleMap: TRuleMap);
begin
  FIndex := -1;
  FList := RuleList; 
  FMap := RuleMap;
  FOnDestroy := nil;
end;

destructor TRules.TEnumerator.Destroy;
begin
  if Assigned(FOnDestroy) then FOnDestroy(Self);
  inherited;
end;

function TRules.TEnumerator.MoveNext: boolean;
begin
  if not Assigned(FList) or not Assigned(FMap) then Exit(False);
  Inc(FIndex);
  Result := FIndex < FList.Count;
end;

initialization

Rules := TRules.Create;

finalization

FreeAndNil(Rules);

end.

