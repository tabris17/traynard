unit Traynard.Launcher;

{$mode ObjFPC}{$H+}
{$MODESWITCH ADVANCEDRECORDS}

interface

uses
  Classes, SysUtils, Generics.Collections, Windows, JwaWinBase, JwaWinNT, TOML,
  Traynard.Types, Traynard.Storage, Traynard.Window;

type

  { TLaunchEntry }

  TLaunchEntry = record
    Name: string;
    Application: string;
    Arguments: string;
    WorkingDirectory: string;
    Notification: TRuleNotification;
    TriggerOn: TLaunchTriggerOn;
    LaunchMethods: TLaunchMethods;
    Hotkey: THotkey;
    Position: TTrayPosition;

    procedure Validate;
    procedure Load(const Config: TConfig);
    procedure Save(const Config: TConfig);
  end;

  { TLauncher }

  TLauncher = class
  type
    TEntry = TLaunchEntry;
    TEntryList = specialize TList<string>;
    TEntryMap = specialize TDictionary<string, TEntry>;

    { THotkeyEntry }

    THotkeyEntry = class
    private
      FHotkeyID: longint;
      FName: string;
    public
      constructor Create(AName: string; AHotkeyID: longint);
      property HotkeyID: longint read FHotkeyID write FHotkeyID;
      property Name: string read FName write FName;
    end;

    THotkeyMap = specialize TObjectDictionary<THotkey, THotkeyEntry>;
    THotkeyPair = specialize TPair<THotkey, THotkeyEntry>;

    { TEnumerator }

    TEnumerator = class
    private
      FIndex: SizeInt;
      FList: TEntryList;
      FMap: TEntryMap;
    protected
      function GetCurrent: TLaunchEntry;
    public
      constructor Create(EntryList: TEntryList; EntryMap: TEntryMap);
      property Current: TLaunchEntry read GetCurrent;
      function MoveNext: boolean;
    end;

    { TProcess }

    TProcess = record
      Name: string;
      PID: DWORD;
      CreationTime: TFileTime;
      Application: string;
      Arguments: string;
    end;

    { TProcessCollection }

    TProcessCollection = class(TPersistent)
    type
      TProcessEnumerator = specialize TEnumerator<TProcess>;
    private
      FProcesses: specialize TDictionary<DWORD, TProcess>;
      function GetProcessCount: integer;
    public
      constructor Create;
      destructor Destroy; override;
      function GetEnumerator: TProcessEnumerator;
      function Find(const PID: DWORD; out Process: TProcess): boolean;
      property ProcessCount: integer read GetProcessCount;
      procedure Add(const PID: DWORD; constref Process: TProcess);
      procedure Remove(const PID: DWORD);
    end;

    { Exception }

    Exception = class(SysUtils.Exception);
  private
    FSelf: TLauncher; static;
    FCurrentPID: DWORD;
    FEntryList: TEntryList;
    FEntryMap: TEntryMap;
    FHotkeyMap: THotkeyMap;
    FConfig: TConfig;
    FConfigEntries: TTOMLArray;
    FOnHotkeyAddedNotify: THotkeyAddedNotify;
    FOnHotkeyRemovedNotify: THotkeyRemovedNotify;
    FProcesses: TProcessCollection;
    function GetCount: SizeInt;
    function GetEntry(Index: SizeInt): TEntry;
    procedure EntryAdded(constref Entry: TEntry); inline;
    procedure EntryRemoved(constref Entry: TEntry); inline;
    procedure EntryUpdated(constref OldEntry, NewEntry: TEntry); inline;
    procedure ProcessExitEvent(Data: PtrInt);
    procedure AutoLaunch;
  public
    constructor Create;
    destructor Destroy; override;
    property Processes: TProcessCollection read FProcesses;
    property Entries[Index: SizeInt]: TEntry read GetEntry; default;
    property Count: SizeInt read GetCount;
    property Hotkeys: THotkeyMap read FHotkeyMap;
    property OnHotkeyAddedNotify: THotkeyAddedNotify read FOnHotkeyAddedNotify write FOnHotkeyAddedNotify;
    property OnHotkeyRemovedNotify: THotkeyRemovedNotify read FOnHotkeyRemovedNotify write FOnHotkeyRemovedNotify;
    function GetEnumerator: TEnumerator;
    function AddEntry(constref Entry: TEntry): SizeInt;
    function HasEntry(const Name: string; out Index: SizeInt): boolean;
    function TryLaunch(constref Entry: TEntry): boolean;
    function TryLaunch(const Name: string): boolean;
    function TryLaunch(const Index: SizeInt): boolean; overload;
    function Find(const Window: TWindow; out Entry: TEntry): boolean;
    procedure Launch(constref Entry: TEntry);
    procedure Launch(const Name: string); overload;
    procedure Launch(const Index: SizeInt); overload;
    procedure Load;
    procedure RemoveEntry(const EntryIndex: SizeInt);
    procedure UpdateEntry(const EntryIndex: SizeInt; constref Entry: TEntry);
    class procedure WaitProcess(Param: PVOID; Fired: ByteBool); stdcall; static;
  end;

const
  CONFIG_NAME = 'launcher';
  ERROR_INVALID_LAUNCHER_ENTRY = 'Invalid launcher entry';

  KEY_ENTRIES = 'entries';
  KEY_NAME = 'name';
  KEY_APPLICATION = 'application';
  KEY_ARGUMENTS = 'arguments';
  KEY_WORKING_DIRECTORY = 'working_directory';
  KEY_LAUNCH_METHODS = 'launch_methods';
  KEY_NOTIFICATION = 'notification';
  KEY_TRIGGER_ON = 'trigger_on';
  KEY_HOTKEY = 'hotkey';
  KEY_POSITION = 'position';

  MAX_WAIT_PROCESSES = 100;

var
  Launcher: TLauncher;

implementation

uses
  Forms, JwaWinternl, Traynard.Helpers, Traynard.Strings;

{ TLaunchEntry }

procedure TLaunchEntry.Validate;
begin
  if (Name = '') or
     (Application = '') or
     (LaunchMethods = [])
  then
    raise Exception.Create(ERROR_INVALID_LAUNCHER_ENTRY);
end;

procedure TLaunchEntry.Load(const Config: TConfig);
var
  Value: TTOMLValue;
begin
  Name := Config.Items[KEY_NAME].AsString; 
  Application := Config.Items[KEY_APPLICATION].AsString;
  if Config.TryGetValue(KEY_ARGUMENTS, Value) then
    Arguments := Value.AsString;
  if Config.TryGetValue(KEY_WORKING_DIRECTORY, Value) then
    WorkingDirectory := Value.AsString;
  Notification := TRuleNotification(Config.Items[KEY_NOTIFICATION].AsInteger);

  LaunchMethods := [];
  for Value in Config.Items[KEY_LAUNCH_METHODS].AsArray.Items do
  begin
    Include(LaunchMethods, TLaunchMethod(Value.AsInteger));
  end;

  TriggerOn := [];
  for Value in Config.Items[KEY_TRIGGER_ON].AsArray.Items do
  begin
    Include(TriggerOn, TLaunchWindowAction(Value.AsInteger));
  end;

  Hotkey.Value := Config.GetInteger(KEY_HOTKEY, 0);
  Position := TTrayPosition(Config.Items[KEY_POSITION].AsInteger);
  Validate;
end;

procedure TLaunchEntry.Save(const Config: TConfig);
var
  Value: TTOMLArray;
  LaunchMethod: TLaunchMethod;
  WindowAction: TLaunchWindowAction;
begin
  Config.Add(KEY_NAME, TOMLString(Name));
  Config.Add(KEY_APPLICATION, TOMLString(Application));
  Config.Add(KEY_ARGUMENTS, TOMLString(Arguments));
  Config.Add(KEY_WORKING_DIRECTORY, TOMLString(WorkingDirectory));
  Config.Add(KEY_NOTIFICATION, TOMLInteger(Ord(Notification)));

  Value := TOMLArray;
  for LaunchMethod := Low(TLaunchMethod) to High(TLaunchMethod) do
  begin
    if LaunchMethod in LaunchMethods then
      Value.Add(TOMLInteger(Ord(LaunchMethod)));
  end;
  Config.Add(KEY_LAUNCH_METHODS, Value);

  Value := TOMLArray;
  for WindowAction := Low(TLaunchWindowAction) to High(TLaunchWindowAction) do
  begin
    if WindowAction in TriggerOn then
      Value.Add(TOMLInteger(Ord(WindowAction)));
  end;
  Config.Add(KEY_TRIGGER_ON, Value);

  Config.Add(KEY_HOTKEY, TOMLInteger(Hotkey.Value));
  Config.Add(KEY_POSITION, TOMLInteger(Ord(Position)));
end;

function TLauncher.GetEntry(Index: SizeInt): TEntry;
begin
  Result := FEntryMap[FEntryList[Index]];
end;

procedure TLauncher.EntryAdded(constref Entry: TEntry);
var
  HotkeyID: longint = HOTKEY_NONE;
begin
  if not (lmHotkey in Entry.LaunchMethods) or (Entry.Hotkey.Value = HOTKEY_NONE) or
     FHotkeyMap.ContainsKey(Entry.Hotkey) then Exit;
  if Assigned(FOnHotkeyAddedNotify) then FOnHotkeyAddedNotify(Entry.Hotkey, HotkeyID);
  FHotkeyMap.Add(Entry.Hotkey, THotkeyEntry.Create(Entry.Name, HotkeyID));
end;

procedure TLauncher.EntryRemoved(constref Entry: TEntry);
var
  HotkeyEntry: THotkeyEntry;
begin
  if not (lmHotkey in Entry.LaunchMethods) or (Entry.Hotkey.Value = HOTKEY_NONE) or
     not FHotkeyMap.TryGetValue(Entry.Hotkey, HotkeyEntry) or
     (HotkeyEntry.Name <> Entry.Name) then Exit;
  if Assigned(FOnHotkeyRemovedNotify) then FOnHotkeyRemovedNotify(HotkeyEntry.HotkeyID);
  FHotkeyMap.Remove(Entry.Hotkey);
end;

procedure TLauncher.EntryUpdated(constref OldEntry, NewEntry: TEntry);
begin
  EntryRemoved(OldEntry);
  EntryAdded(NewEntry);
end;

procedure TLauncher.ProcessExitEvent(Data: PtrInt);
begin
  FProcesses.Remove(DWORD(Data));
  FProcesses.FPONotifyObservers(Self, ooDeleteItem, Pointer(Data));
end;

procedure TLauncher.AutoLaunch;
var
  Entry: TEntry;
begin
  for Entry in FEntryMap.Values do
  begin
    if not (lmAutomatic in Entry.LaunchMethods) then Continue;
    TryLaunch(Entry);
  end;
end;

function TLauncher.GetCount: SizeInt;
begin
  Result := FEntryList.Count;
end;

constructor TLauncher.Create;
begin
  FOnHotkeyAddedNotify := nil;
  FOnHotkeyRemovedNotify := nil;
  FEntryList := TEntryList.Create;
  FEntryMap := TEntryMap.Create;
  FHotkeyMap := THotkeyMap.Create([doOwnsValues]);
  FProcesses := TProcessCollection.Create;
  FSelf := Self;
  FCurrentPID := DWORD(System.GetProcessID);
end;

destructor TLauncher.Destroy;
begin
  if Assigned(FConfig) then
  begin
    Storage.SaveConfig(CONFIG_NAME, FConfig);
    FreeAndNil(FConfig);
  end;

  FreeAndNil(FEntryList);
  FreeAndNil(FEntryMap);
  FreeAndNil(FHotkeyMap);
  FreeAndNil(FProcesses);

  inherited Destroy;
end;

function TLauncher.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(FEntryList, FEntryMap);
end;

function TLauncher.AddEntry(constref Entry: TEntry): SizeInt;
var
  ConfigEntry: TConfig;
begin
  Entry.Validate;
  FEntryMap.Add(Entry.Name, Entry);
  Result := FEntryList.Add(Entry.Name);
  EntryAdded(Entry);

  ConfigEntry := TOMLTable;
  Entry.Save(ConfigEntry);
  FConfigEntries.Add(ConfigEntry);

  Storage.SaveConfig(CONFIG_NAME, FConfig);
end;

function TLauncher.HasEntry(const Name: string; out Index: SizeInt): boolean;
begin
  Result := FEntryMap.ContainsKey(Name);
  if Result then
    Index := FEntryList.IndexOf(Name);
end;

function TLauncher.TryLaunch(constref Entry: TEntry): boolean;
begin
  try
    Launch(Entry);
    Result := True;
  except
    Result := False;
  end;
end;

function TLauncher.TryLaunch(const Name: string): boolean;
begin
  Result := TryLaunch(FEntryMap[Name]);
end;

function TLauncher.TryLaunch(const Index: SizeInt): boolean;
begin
  Result := TryLaunch(FEntryList[Index]);
end;

function TLauncher.Find(const Window: TWindow; out Entry: TEntry): boolean;
var
  Process: TProcess;
  ProcessBasicInfo: Traynard.Types.TProcessBasicInformation;
  ReturnLength: ULONG;
  ProcessHandle: HANDLE;
  ProcessID: DWORD;
  CreationTime, _NoUseTime: FILETIME;
begin
  ProcessID := Window.PID;

  if FProcesses.Find(ProcessID, Process) then
  begin
    Result := FEntryMap.TryGetValue(Process.Name, Entry);
  end
  else
  begin
    repeat
      if ProcessID = FCurrentPID then Exit(False);

      ProcessHandle := OpenProcess(PROCESS_QUERY_INFORMATION, False, ProcessID);
      if ProcessHandle = 0 then Exit(False);

      if not GetProcessTimes(ProcessHandle, CreationTime,
                             _NoUseTime, _NoUseTime, _NoUseTime) or
         (NtQueryInformationProcess(ProcessHandle,
                                   ProcessBasicInformation,
                                   @ProcessBasicInfo,
                                   SizeOf(ProcessBasicInfo),
                                   @ReturnLength) <> 0) then
      begin
        CloseHandle(ProcessHandle);
        Exit(False);
      end;

      CloseHandle(ProcessHandle);
      ProcessID := DWORD(ProcessBasicInfo.InheritedFromUniqueProcessId);
    until FProcesses.Find(ProcessID, Process);

    if CompareFileTime(Process.CreationTime, CreationTime) > 0 then
      Exit(False);

    Result := FEntryMap.TryGetValue(Process.Name, Entry);
  end;
end;

procedure TLauncher.Launch(constref Entry: TEntry);
var
  ProcessInfo: TProcessInformation;
  StartupInfo: TStartupInfoW;
  CmdLine: PWideChar = nil;
  CurrDir: PWideChar = nil;
  WaitHandle: HANDLE;
  Process: TProcess;
  CreationTime, _NoUseTime: TFileTime;
  Exc: Exception;
begin
  if FProcesses.ProcessCount >= MAX_WAIT_PROCESSES then
    raise Exception.Create(ERROR_MAX_LAUNCHES);

  FillChar(StartupInfo, SizeOf(StartupInfo), 0);
  StartupInfo.cb := SizeOf(StartupInfo);
  if Entry.Arguments <> '' then
    CmdLine := PWideChar(UnicodeString(Entry.Arguments));
  if Entry.WorkingDirectory <> '' then
    CurrDir := PWideChar(UnicodeString(Entry.WorkingDirectory));

  if not CreateProcessW(PWideChar(UnicodeString(Entry.Application)),
                        CmdLine,
                        nil,
                        nil,
                        False,
                        CREATE_NEW_CONSOLE or CREATE_NEW_PROCESS_GROUP,
                        nil,
                        CurrDir,
                        StartupInfo,
                        ProcessInfo) then raise Exception.Create(GetLastErrorMsg);

  CloseHandle(ProcessInfo.hThread);

  if not RegisterWaitForSingleObject(WaitHandle,
                                     ProcessInfo.hProcess,
                                     @WaitProcess,
                                     PVOID(PtrUInt(ProcessInfo.dwProcessId)),
                                     INFINITE,
                                     WT_EXECUTEONLYONCE) then
  begin
    Exc := Exception.Create(GetLastErrorMsg);
    CloseHandle(ProcessInfo.hProcess);
    raise Exc;
  end;

  if not GetProcessTimes(ProcessInfo.hProcess, CreationTime, _NoUseTime, _NoUseTime, _NoUseTime) then
  begin
    Exc := Exception.Create(GetLastErrorMsg);
    UnregisterWait(WaitHandle);
    CloseHandle(ProcessInfo.hProcess);
    raise Exc;
  end;

  Process.Name := Entry.Name;              
  Process.Application := Entry.Application;
  Process.Arguments := Entry.Arguments;
  Process.PID := ProcessInfo.dwProcessId;
  Process.CreationTime := CreationTime;
  FProcesses.Add(Process.PID, Process);
  FProcesses.FPONotifyObservers(Self, ooAddItem, Pointer(PtrUInt(Process.PID)));
end;

procedure TLauncher.Launch(const Name: string);
var
  Entry: TEntry;
begin
  Entry := FEntryMap[Name];
  Launch(Entry);
end;

procedure TLauncher.Launch(const Index: SizeInt);
begin
  Launch(FEntryList[Index]);
end;

procedure TLauncher.Load;
var
  ConfigEntries, ConfigEntry: TTOMLValue;
  Entry: TEntry;
begin
  if Assigned(FConfig) then
    raise Exception.Create('Duplicate launcher entries loading');

  Storage.LoadConfig(CONFIG_NAME, FConfig);
  if FConfig.TryGetValue(KEY_ENTRIES, ConfigEntries) then
  begin
    if ConfigEntries is TTOMLArray then
    begin
      FConfigEntries := ConfigEntries as TTOMLArray;
      for ConfigEntry in FConfigEntries.Items do
      begin
        try
          Entry.Load(ConfigEntry as TTOMLTable);
        except
          Continue;
        end;
        if FEntryMap.TryAdd(Entry.Name, Entry) then
        begin
          FEntryList.Add(Entry.Name);
          EntryAdded(Entry);
        end;
      end;
    end
    else
    begin
      FConfig.Items.Remove(KEY_ENTRIES);
      ConfigEntries.Free;
      FConfigEntries := TOMLArray;
      FConfig.Add(KEY_ENTRIES, FConfigEntries);
    end;
  end
  else
  begin
    FConfigEntries := TOMLArray;
    FConfig.Add(KEY_ENTRIES, FConfigEntries);
  end;

  AutoLaunch;
end;

procedure TLauncher.RemoveEntry(const EntryIndex: SizeInt);
var
  EntryName: string;
  Entry: TEntry;
  ConfigEntry: TTOMLValue;
begin
  EntryName := FEntryList[EntryIndex];
  Entry := FEntryMap[EntryName];
  FEntryList.Delete(EntryIndex);
  FEntryMap.Remove(EntryName);
  EntryRemoved(Entry);

  ConfigEntry := FConfigEntries.Items[EntryIndex];
  FConfigEntries.Items.Remove(ConfigEntry);
  ConfigEntry.Free;

  Storage.SaveConfig(CONFIG_NAME, FConfig);
end;

procedure TLauncher.UpdateEntry(const EntryIndex: SizeInt; constref Entry: TEntry);
var
  OldConfigEntry: TTOMLValue;
  NewConfigEntry: TConfig;
  OldEntry: TEntry;
  OldEntryName: string;
begin
  OldEntryName := FEntryList[EntryIndex];
  OldEntry := FEntryMap[OldEntryName];
  Entry.Validate;
  if OldEntryName = Entry.Name then
  begin
    FEntryMap[Entry.Name] := Entry;
  end
  else
  begin
    FEntryMap.Remove(OldEntryName);
    FEntryMap.Add(Entry.Name, Entry);
    FEntryList[EntryIndex] := Entry.Name;
  end;
  EntryUpdated(OldEntry, Entry);

  OldConfigEntry := FConfigEntries.Items[EntryIndex];
  FConfigEntries.Items.Remove(OldConfigEntry);
  OldConfigEntry.Free;
  NewConfigEntry := TOMLTable;
  Entry.Save(NewConfigEntry);
  FConfigEntries.Items.Insert(EntryIndex, NewConfigEntry);

  Storage.SaveConfig(CONFIG_NAME, FConfig);
end;

class procedure TLauncher.WaitProcess(Param: PVOID; Fired: ByteBool); stdcall;
begin
  Application.QueueAsyncCall(@FSelf.ProcessExitEvent, PtrInt(Param));
end;

{ TLauncher.THotkeyEntry }

constructor TLauncher.THotkeyEntry.Create(AName: string; AHotkeyID: longint);
begin
  FName := AName;
  FHotkeyID := AHotkeyID;
end;

{ TLauncher.TEnumerator }

function TLauncher.TEnumerator.GetCurrent: TLaunchEntry;
begin
  Result := FMap[FList[FIndex]];
end;

constructor TLauncher.TEnumerator.Create(EntryList: TEntryList; EntryMap: TEntryMap);
begin
  FIndex := -1;
  FList := EntryList;
  FMap := EntryMap;
end;

function TLauncher.TEnumerator.MoveNext: boolean;
begin
  if not Assigned(FList) or not Assigned(FMap) then Exit(False);
  Inc(FIndex);
  Result := FIndex < FList.Count;
end;

{ TLauncher.TProcessCollection }

function TLauncher.TProcessCollection.GetProcessCount: integer;
begin
  Result := FProcesses.Count;
end;

constructor TLauncher.TProcessCollection.Create;
begin
  FProcesses := specialize TDictionary<DWORD, TProcess>.Create;
end;

destructor TLauncher.TProcessCollection.Destroy;
begin
  inherited Destroy;

  FreeAndNil(FProcesses);
end;

function TLauncher.TProcessCollection.GetEnumerator: TProcessEnumerator;
begin
  Result := FProcesses.Values.GetEnumerator;
end;

function TLauncher.TProcessCollection.Find(const PID: DWORD; out Process: TProcess): boolean;
begin
  Result := FProcesses.TryGetValue(PID, Process);
end;

procedure TLauncher.TProcessCollection.Add(const PID: DWORD; constref Process: TProcess);
begin
  FProcesses.Add(PID, Process);
end;

procedure TLauncher.TProcessCollection.Remove(const PID: DWORD);
begin
  FProcesses.Remove(PID);
end;

initialization

Launcher := TLauncher.Create;

finalization

FreeAndNil(Launcher);

end.

