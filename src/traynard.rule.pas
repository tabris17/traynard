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

    procedure Validate;
    procedure Load(const Config: TConfig);
    procedure Save(const Config: TConfig);
  end;

  { TRules }

  TRules = class
  type
    TRuleList = specialize TList<TRule>;
    TRuleEnumerator = specialize TEnumerator<TRule>;
  private
    FRules: TRuleList;
    FRegEx: TRegExpr;
    FConfig: TConfig;
    FConfigRules: TTOMLArray;
    function GetCount: SizeInt;
    function GetRule(Index: SizeInt): TRule;
  public
    constructor Create;
    destructor Destroy; override;
    property Rules[Index: SizeInt]: TRule read GetRule; default;
    property Count: SizeInt read GetCount;
    function Match(const Window: TWindow; out Rule: TRule; WindowAction: TWindowAction): boolean;
    function AddRule(Rule: TRule): integer;
    function GetEnumerator: TRuleEnumerator;
    procedure Load;
    procedure RemoveRule(RuleIndex: integer);
    procedure UpdateRule(RuleIndex: integer; Rule: TRule);
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

implementation

uses
  LazLogger, TOML;

{ TRule }

procedure TRule.Validate;
var
  WindowActions: TRuleTriggerOn;
begin
  WindowActions := [waCreation, waChange];
  if TriggerOn * WindowActions = [] then
    Exclude(TriggerOn, waExisting)
  else
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
  for WindowAction := Low(TWindowAction) to High(TWindowAction) do
  begin
    if WindowAction in TriggerOn then
      Value.Add(TOMLInteger(Ord(WindowAction)));
  end;
  Config.Add(KEY_TRIGGER_ON, Value);

  Config.Add(KEY_NOTIFICATION, TOMLInteger(Ord(Notification)));
  Config.Add(KEY_POSITION, TOMLInteger(Ord(Position)));
end;

{ TRules }

function TRules.GetRule(Index: SizeInt): TRule;
begin
  Result := FRules[Index];
end;

function TRules.GetEnumerator: TRuleEnumerator;
begin
  Result := FRules.GetEnumerator;
end;

function TRules.GetCount: SizeInt;
begin
  Result := FRules.Count;
end;

constructor TRules.Create;
begin
  FRules := TRuleList.Create;
  FRegEx := TRegExpr.Create;
end;

destructor TRules.Destroy;
begin
  if Assigned(FConfig) then
  begin
    Storage.Save(CONFIG_NAME, FConfig);
    FreeAndNil(FConfig);
  end;

  FreeAndNil(FRules);
  FreeAndNil(FRegEx);

  inherited Destroy;
end;

function TRules.Match(const Window: TWindow; out Rule: TRule; WindowAction: TWindowAction): boolean;

  function Equal(const RuleText: TRuleText; const Text: string): boolean;
  begin
    case RuleText.Comparison of 
      rtcAny:        Exit(True);
      rtcEquals:     Exit(RuleText.Text = Text);
      rtcContains:   Exit(Pos(RuleText.Text, Text) > 0);
      rtcStartsWith: Exit(Text.StartsWith(RuleText.Text));
      rtcEndsWith:   Exit(Text.EndsWith(RuleText.Text));
      rtcRegexMatch:
      begin
        FRegEx.Expression := RuleText.Text;
        FRegEx.InputString := Text;
        Exit(FRegEx.Exec);
      end;
    end;
    Result := False;
  end;

begin
  for Rule in FRules do
  begin
    if (WindowAction in Rule.TriggerOn) and
       Equal(Rule.WindowTitle, Window.Text) and
       Equal(Rule.WindowClass, Window.ClassName) and
       Equal(Rule.AppPath, Window.AppPath) then Exit(True);
  end;
  Result := False;
end;

function TRules.AddRule(Rule: TRule): integer;
var
  ConfigRule: TConfig;
begin
  Rule.Validate;
  Result := FRules.Add(Rule);

  ConfigRule := TOMLTable;
  Rule.Save(ConfigRule);
  FConfigRules.Add(ConfigRule);
end;

procedure TRules.Load;
var
  ConfigRules, ConfigRule: TTOMLValue;
  Rule: TRule;
begin
  Storage.Load(CONFIG_NAME, FConfig);

  if FConfig.TryGetValue(KEY_RULES, ConfigRules) then
  begin
    if ConfigRules is TTOMLArray then
    begin
      FConfigRules := ConfigRules as TTOMLArray;
      for ConfigRule in FConfigRules.Items do
      begin
        try
          Rule.Load(ConfigRule as TTOMLTable);
          FRules.Add(Rule);
        except
          Continue;
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

procedure TRules.RemoveRule(RuleIndex: integer);
var
  ConfigRule: TTOMLValue;
begin
  FRules.Delete(RuleIndex);

  ConfigRule := FConfigRules.Items[RuleIndex];
  FConfigRules.Items.Remove(ConfigRule);
  ConfigRule.Free;
end;

procedure TRules.UpdateRule(RuleIndex: integer; Rule: TRule);
var
  OldConfigRule: TTOMLValue;
  NewConfigRule: TConfig;
begin
  Rule.Validate;
  FRules.Items[RuleIndex] := Rule;

  OldConfigRule := FConfigRules.Items[RuleIndex];
  FConfigRules.Items.Remove(OldConfigRule);
  OldConfigRule.Free;
  NewConfigRule := TOMLTable;
  Rule.Save(NewConfigRule);
  FConfigRules.Items.Insert(RuleIndex, NewConfigRule);
end;

initialization

Rules := TRules.Create;

finalization

FreeAndNil(Rules);

end.

