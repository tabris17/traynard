# Traynard 增量代码审查报告（f783ce92 之后）

| 项目 | 内容 |
|------|------|
| 项目 | Traynard（Windows 托盘最小化工具，Lazarus / Free Pascal） |
| 审查日期 | 2026-09-01 |
| 审查范围 | `f783ce92..HEAD`，共 19 个提交、20 个文件（+845 / −56） |
| 基线 | `f783ce9` feat(ci): download Chinese language files for Inno Setup installer（2026-07-21） |
| 审查方法 | 逐提交全量 diff 审查 + 7 个并行角度审查代理 + 逐项人工复核；关键结论经 LCL 4.8 源码比对、`msgfmt` 实测与最小化 FPC 复现实验验证 |

---

## 一、执行摘要

本次审查覆盖基线之后的全部变更：一系列崩溃修复（空托盘守卫、弹窗淡入淡出、OpenProcess 空句柄、i18n 头解析、热键注销越界）、资源泄漏修复（GDI 区域、进程/等待句柄）、热键管理器析构、启动器进程句柄生命周期管理，以及繁体中文本地化更新。

大部分修复扎实、针对性强，且确实修复了上一轮全量审查（`code-review-report.md`）中的已知缺陷（见 §四 对照表）。但发现 **2 个严重新缺陷** 与 **3 个中等问题**：

| 严重程度 | 数量 | 说明 |
|----------|------|------|
| 🔴 严重 (Critical) | 2 | 每次正常退出必然 AV（新析构函数顺序错误）；发布版静默丢失两个繁体语言包（PO 语法错误） |
| 🟠 高 (High) | 1 | 页面 Finalize 同类关机顺序隐患（低概率触发路径） |
| 🟡 中 (Medium) | 2 | PID 复用竞态；句柄清理三处复制已分叉 |
| 🟢 低/既有 | 4 | 均为基线已存在、本次未引入的遗留缺陷（列出供顺手修复） |

**最需优先修复的问题：**

1. **N-1（🔴）** `THotkeyManager.Destroy` 解引用已释放的 `Settings`/`Rules`/`Launcher` —— 每次正常退出必然访问违规，且是本次变更新引入的。
2. **N-2（🔴）** `zh_HK.po` / `zh_TW.po` 两处 `msgstr` 含裸换行，`msgfmt` 编译失败 —— 发布包将静默缺失这两种语言。
3. **N-3（🟠）** `TPageLauncher.Finalize` 在非正常退出路径（注销/关机）下有同类单例解引用风险。

---

## 二、新发现问题（本次变更引入）

### N-1 🔴 `THotkeyManager.Destroy` 关机时必然访问已释放单例

- **位置**：`src/traynard.hotkey.pas:241-259`（提交 `1c9dabf`）
- **类型**：释放后使用 / nil 解引用，**每次正常退出必然触发**

析构函数开头依次引用三个单元单例：

```pascal
destructor THotkeyManager.Destroy;
begin
  Settings.RemoveListeners(Self);            // ← Settings 已为 nil
  Rules.OnHotkeyAddedNotify := nil;          // ← Rules 已释放
  Launcher.OnHotkeyAddedNotify := nil;       // ← Launcher 已释放
  ...
```

**原因（单元终结顺序）**：

1. `HotkeyManager` 由 `Application.MainForm`（FormBackground）拥有（`traynard.lpr:51`），只在 Forms 单元 finalization 释放 Application 时随之销毁。已核对安装的 LCL 4.8 源码：`lcl/forms.pp:2435` `FreeThenNil(Application)`。
2. FPC 的 finalization 按初始化的严格逆序执行。所有 Traynard 单元都（直接或间接）依赖 Forms，因此全部**先于** Forms 终结：`Traynard.Settings`（`finalization FreeAndNil(Settings)`）、`Traynard.Rule`、`Traynard.Launcher` 早已把单例释放并置 **nil**。
3. 随后 Forms finalization 销毁 FormBackground → 销毁其拥有的组件 → `THotkeyManager.Destroy` 第一行 `Settings.RemoveListeners(Self)` 对 nil 对象取字段 → **Access Violation**。

**实证**：构造了一个与本仓库依赖形状完全一致的最小复现（unit A 模拟 Forms 并在 finalization 释放"Application"；unit B interface-uses A、implementation-uses C；unit C 模拟 Settings；program uses 顺序同 `traynard.lpr`）。FPC 3.2.2 实际运行输出：

```
fini uSettings (FreeAndNil(Settings))      ← Settings 先被释放
fini uForms (FreeThenNil(Application))     ← 随后才释放 Application
  THotkeyManager.Destroy: Settings pointer = 0   ← 析构时 Settings 已是 nil
```

**佐证与细节**：

- 本项目自己的提交 `59583fe`（启动器句柄修复）在 `TProcessExitDispatcher` 注释中明确写着"Forms finalization 发生在本单元 finalization 释放 Launcher 之后"——作者已认识到该顺序，但更早提交的热键析构未遵守同一约束。
- 析构中的 `UnregisterHotkey(FMainForm.Handle, …)` 循环是**死代码**：LCL `TWinControl.Destroy`（`lcl/include/wincontrol.inc:6652`）在销毁拥有组件**之前**就调用 `DestroyHandle`；`csDestroying` 状态下 `HandleNeeded` 为空操作（`wincontrol.inc:7925`），`Handle` 返回 0。且窗口销毁时 Windows 已自动注销该窗口的全部热键。
- **同样的隐患在先前的 `TTrayManager.Destroy` 与 `TWindowManager.Destroy` 中同样存在**（均调用 `Settings.RemoveListeners(Self)`，自初始提交即有）。组件按创建逆序销毁（Hotkey → Tray → Window），目前崩溃点在新加的 `THotkeyManager.Destroy`；若只修它，崩溃点会移到 `TTrayManager.Destroy`。

**建议修复**：三处析构统一加守卫（或与 `TProcessExitDispatcher` 同思路）：

```pascal
if Assigned(Settings) then Settings.RemoveListeners(Self);
if Assigned(Rules) then begin Rules.OnHotkeyAddedNotify := nil; ... end;
if Assigned(Launcher) then begin Launcher.OnHotkeyAddedNotify := nil; ... end;
```

（析构时机决定了单例要么已死、要么事件已无意义，守卫分支实际不会执行；更彻底的做法是把这些管理器在运行期、单例尚存活时显式释放，或统一放弃 finalization 期清理。）

---

### N-2 🔴 zh_HK.po / zh_TW.po PO 语法错误，发布版静默缺失两种语言

- **位置**：`locale/zh_HK.po:573,593`、`locale/zh_TW.po:573,593`（提交 `5550ddf`、`4b9ae5f`）
- **类型**：构建产物缺陷

两个文件各有两处 `msgstr` 在双引号字符串内直接换行（`tpageoptions.checkboxmultiprocesslaunch.hint`、`tpageoptions.checkboxrunasadmin.hint`）：

```po
msgstr "同時監控目標程序的子程序。
適用於類似基於 Electron.js 開發的應用程式。"
```

PO 格式不允许字符串内出现裸换行。实测 `msgfmt` 对这两个文件报 `syntax error`（`zh_CN.po` 校验通过）。

**影响**：`.github/workflows/release.yml:70-73` 逐个用 `msgfmt` 把 `.po` 编译为 `.mo`：

```powershell
foreach ($po in Get-ChildItem $localeDir -Filter "*.po") {
    & $msgfmt $po.FullName -o (Join-Path $outDir "$name.mo")
}
```

循环无错误检查——两个文件编译失败被静默跳过，**`zh_HK.mo` / `zh_TW.mo` 不会进入安装包**，繁体用户回退到英文。

**建议修复**：改为两行拼接形式（或单行内 `\n`）：

```po
msgstr "同時監控目標程序的子程序。\n"
       "適用於類似基於 Electron.js 開發的應用程式。"
```

并建议在 CI 中对所有 `.po` 增加 `msgfmt --check` 校验步骤，防止回归。

---

### N-3 🟠 `TPageLauncher.Finalize` 同类关机顺序隐患（低概率路径）

- **位置**：`src/traynard.page.launcher.pas:176-180`（提交 `bcbc0c9`）
- **类型**：释放后使用（条件触发）

```pascal
procedure TPageLauncher.Finalize;
begin
  Settings.RemoveListeners(Self);   // Settings 可能已为 nil
  inherited Finalize;
end;
```

**正常退出路径安全**：托盘菜单退出 → `FormBackground.FormCloseQuery` → `FormMain.Close`（`FormClose` 设 `CloseAction := caFree`）在运行期释放主窗体及页面，此时 `Settings` 仍存活。

**风险路径**：若 FormMain 未经关闭流程就在 Forms finalization 中被销毁（系统注销/关机 WM_ENDSESSION、异常路径 `Application.Terminate` 等），页面析构时 `Settings` 已为 nil → AV；若当时启动器页可见，`TFramePage.Destroy → DoPageDeactivate → PageDeactivate` 还会解引用已置 nil 的 `Launcher`（`Launcher.Processes.FPODetachObserver(Self)`）。

**建议修复**：与 N-1 一并加 `Assigned(Settings)` / `Assigned(Launcher)` 守卫。

---

### N-4 🟡 启动器按 PID 单键跟踪进程，存在 PID 复用竞态

- **位置**：`src/traynard.launcher.pas:288-299、498-514`（提交 `59583fe`）
- **类型**：竞态条件（窗口窄，但后果具体）

退出事件以 PID 为参数经 `Application.QueueAsyncCall` 排队。若旧进程的退出事件尚未派发时，新启动的进程恰好复用同一 PID：

1. **启动侧**：`FProcesses.Add(PID, …)` 因键重复抛异常 → `except` 分支注销新等待句柄、关闭新进程句柄后重新抛出 → `TryLaunch` 静默返回 False（用户启动失败且无任何提示）。
2. **退出侧**：随后旧事件派发，`ProcessExitEvent` 命中**新**条目，注销新进程的等待句柄、关闭新句柄并删除条目——新进程从此脱离跟踪（托盘联动、多进程匹配均失效）。

窗口很窄（需主线程繁忙 + PID 复用同时发生），但新代码在旧竞态之上多了"误清理新条目句柄"一层。旧代码（仅 `Remove`）同样有此竞态，故定级中等。

**建议修复**：用自增内部 id（而非系统 PID）作 `TProcessCollection` 键，回调参数携带该 id；或至少在 `ProcessExitEvent` 中用 `CreationTime` 比对（条目已存该字段）确认是同一进程再清理。

---

### N-5 🟡 句柄清理序列三处手写复制，且已开始分叉

- **位置**：`src/traynard.launcher.pas:294-295`（`ProcessExitEvent`）、`511-512`（`Launch` except 回滚）、`688-689`（`TProcessCollection.Destroy`）
- **类型**：可维护性 / 复用

`UnregisterWait + CloseHandle` 释放序列出现在三处，且已经分叉：前两处用 `UnregisterWait`，Destroy 用阻塞式 `UnregisterWaitEx(…, INVALID_HANDLE_VALUE)`。将来 `TProcess` 若新增第三种自有资源，或释放顺序需要修正，三处必须手工同步，漏改即泄漏。

同类模式：`LastWindow` 前的空托盘守卫在 `traynard.form.background.pas:133` 与 `traynard.page.tray.pas:88` 逐字重复；两处其实都可以直接复用已有的 `TWindowManager.TryRestoreLastWindow`（它已内置空集合与异常处理，见 `traynard.window.pas:1355` 附近）。

**建议修复**：为 `TProcess` 提供单一 `Release` 辅助方法供三处调用；空托盘守卫下沉为集合自身契约（如 `TryGetLastWindow(out Window)`）。

---

### N-6 🟢 其他轻微项

| # | 位置 | 说明 |
|---|------|------|
| N-6a | `traynard.window.pas:302`（`GetAppPath`） | `OpenProcess` 失败时 `Exit('')` 未把空结果缓存进 `FAppPath`（`HasValue` 保持 False），受保护进程的窗口每次访问 AppPath 都会重试系统调用。极轻微，可选缓存空值 |
| N-6b | `traynard.form.popup.pas:49-52`（`TimerCloseTimer`） | 只启用 `TimerFadeOut`、未停 `TimerFadeIn`。若将来有 < ~340ms（淡入总时长）的 Timeout，两定时器 +15/−15 抵消，弹窗卡在中间值永不释放。当前所有调用方用默认 2500ms，不触发；建议顺手在 `TimerCloseTimer` 中加 `TimerFadeIn.Enabled := False` |
| N-6c | `traynard.form.popup.pas:60`（`Release`） | `Release` 到真正释放之间弹窗仍在 `FormMain.FPopupList` 中，此窗口期内新弹窗 `PopupShow` 叠放会短暂多出幻影空隙。影响极小，仅记录 |
| N-6d | `traynard.launcher.pas:692-694` | `TProcessCollection.Destroy` 中 `inherited Destroy` 放在 `FreeAndNil(FProcesses)` 之前——对 `TPersistent` 无害，仅风格提示 |
| N-6e | `traynard.form.main.pas:284-287`、`305-309` | 效率类：`Navigate` 的 `Contains`+`Remove` 双重线性扫描冗余（`Remove` 对不存在项即空操作，`Backward` 已直接用 `Remove`）；`ProcessExitEvent` 的 `Find`+`Remove` 可合并为一次 `ExtractPair` |

---

## 三、基线既有缺陷（本次变更未引入，列出供顺手修复）

以下问题在基线 `f783ce92` 已存在，本次 diff 未引入；其中 N-7、N-8 与上一轮全量审查报告的发现相互印证。

### N-7 🟠 规则编辑器：保存新规则写 ListBox 越界（= 全量报告 PR-1）

- **位置**：`src/traynard.page.rules.pas` `ActionSaveExecute` esNew 分支（约 495-510 行）
- `RuleIndex := Rules.AddRule(Rule)` 之后 `ListBoxRules.Items[RuleIndex] := Rule.Name`，但无任何代码向 `ListBoxRules` 添加过新项（全文件仅 `Initialize` 的加载循环有 `Items.Add`）→ 索引 == `Count` → `EListError`。
- 本次未改动该逻辑，但新增的 `ActionCloseExecute` IDYES 分支（`ActionSaveExecute(Sender); if Unsaved then Exit;`）会走到它：新建规则后点关闭 → 选"保存" → 异常弹窗、关闭被中止。

### N-8 🟡 `TLauncher.Load` 跳过非法条目导致索引漂移（= 全量报告 L-3）

- **位置**：`src/traynard.launcher.pas:545-558` 与 `RemoveEntry`/`UpdateEntry`
- `Load` 对非法/重名条目 `Continue` / `TryAdd` 失败，但 `FConfigEntries` 保留全部条目 → `FEntryList` 与 `FConfigEntries.Items` 索引错位；`RemoveEntry(EntryIndex)`/`UpdateEntry` 用同一索引操作 `FConfigEntries.Items[EntryIndex]`，删除/替换的是错误的 TOML 节点（被删条目重启后复活、错删他条）。本次新增的 `Entry := Default(TEntry)` 修复了字段串味，但漂移仍在。

### N-9 🟡 `TryRestoreAllWindows` 遍历中被修改

- **位置**：`src/traynard.window.pas` `RestoreWindow` / `TryRestoreAllWindows`（约 1252-1310 行）
- `RestoreWindow` 的"窗口未隐藏"分支从 `FTray.FWindows` 中 `Remove` 并抛异常，而 `TryRestoreAllWindows` 正在遍历 `FWindows.Values` 并吞异常继续 → 枚举器失效，可能漏恢复窗口。本次 diff 只修改了该分支的注释（注释本身修正是正确的）。

### N-10 🟢 `Unregister` 缺负值检查（理论项）

- **位置**：`src/traynard.hotkey.pas:290`
- 本次把 `>` 修为 `>=`（正确），但仍只查上界。经查 `HOTKEY_NONE = 0 = TEST_HOTKEY_ID`（`traynard.types.pas:246`），所有现行调用路径的 ID ≥ 0，负值路径不存在；仅记录以防未来引入。

---

## 四、验证通过的变更（确认正确）

| 提交 | 变更 | 验证结论 |
|------|------|----------|
| `59583fe` | 启动器进程/等待句柄生命周期管理 | 核心目标达成（修复全量报告 L-1 句柄泄漏）：`CreateProcessW` 失败路径、`RegisterWaitForSingleObject` 失败路径、`GetProcessTimes` 失败路径、`FProcesses.Add` 回滚路径均完整清理；`TProcessExitDispatcher` 故意不释放 + `Assigned(Launcher)` 守卫的设计对其目标场景正确。悬垂指针修复（全量报告 L-2）正确：`PWideChar` 现在指向存活至 `CreateProcessW` 返回的局部 `UnicodeString`。残留问题见 N-4、N-5 |
| `59583fe` | `Entry := Default(TEntry)` | 正确修复跨条目字段串味（`Arguments`/`WorkingDirectory` 为条件加载）；赋值语义会正常终结旧字符串，无泄漏 |
| `1c9dabf` / `20c6596` | `THotkeyManager.Unregister` 边界 `>` → `>=` | 正确：旧代码允许 `HotkeyID = Length(FHotkeys)` 越界访问；`HOTKEY_NONE = 0` 由 `TEST_HOTKEY_ID` 分支拦截 |
| `1c9dabf` | `THotkeyManager.Destroy` | 意图正确（注销监听/热键、还原 WindowProc），但触发时机错误，见 N-1 |
| `bcbc0c9` | 启动器页 `EnableLauncher` 监听 | `Settings.AddListener(siUseLauncher, …)` 与 `RemoveListeners(Self)`（按对象剥离 `TMethod`）配对正确；菜单索引 1..Count-1 与 LFM 中 `MenuItemEmpty` 为第 0 项一致；`TFramePage.Destroy` 调用 `Finalize` 仅一次 |
| `629d979` | 弹窗 `Free` → `Release` | 正确避免在自身事件处理器中自我释放导致的 AV |
| `7ad47df` | 淡入淡出步进钳制 | 数学验证：STEP=15，淡出 `<=15` 归零、淡入 `>=241` 归 255，两方向均不会越界/永不终止 |
| `ad9b0bd` | `FormHide` 空守卫 / `ToolBarResize` 负高度钳制 / `Navigate` 跳过 `piNone` | 均正确。注：`SetCurrentPageIndex(piNone)` 仍会 AV，但无任何调用方传 `piNone`，属既有哨兵值结构问题 |
| `191e917` / `8a6b6fc` | 空托盘 `WindowCount = 0` 守卫（背景窗体、托盘页） | 正确；重复实现问题见 N-5 |
| `ffab6cd` | `GetAppPath` 中 `OpenProcess` 空句柄检查 | 正确（轻微缓存问题见 N-6a） |
| `d355c07` | `TWindowCollection.Destroy` 补 `inherited Destroy`；ShowWindow 分支注释修正 | 均正确（注释新文义与 `ShowWindow(SW_SHOW)` 返回 TRUE = "原本可见"一致） |
| `6604cd6` | 模糊背景 `DeleteObject(Rgn)` | 正确：`DwmEnableBlurBehindWindow` 复制区域，调用方负责释放；位置在 `Rgn <> 0` 守卫内且其后未再使用（修复全量报告相关 GDI 泄漏） |
| `61638ce` | i18n 头解析守卫 | 正确：`Line.Split(':', 2)` 无冒号时返回长度 1 数组，守卫后 `Continue`；值中含冒号仍保留在第二段 |
| `c4e27bf` / `c5ba9f8` | helpers 重复赋值清理 / desktop 双分号 | 正确 |
| `ba6f593` | 托盘菜单反向遍历删除 | 行为等价（原代码删后即 `Break`）且不再于 for-in 遍历中修改集合，更安全 |
| `7a2d89d` | 规则编辑器状态处理 + "Window Titile" 拼写 | `LabelHotkey` 失败态复位（`clDefault` + `ShowHint := False`）正确；`ActionCloseExecute` IDYES 保存失败即中止（`if Unsaved then Exit`）语义正确；lfm/lrj/pot/zh_CN 拼写联动一致 |
| `5550ddf` / `4b9ae5f` | zh_HK/zh_TW 翻译补全与 fuzzy 清理 | 内容完整，但存在 N-2 的语法错误 |
| — | `locale/traynard.pot`、`locale/zh_CN.po` | `msgfmt --check` 通过，`msgcmp` 仅剩与本次无关的历史未翻译项 |

---

## 五、与上一轮全量审查（2026-08-10）的对照

| 全量报告编号 | 状态 | 说明 |
|--------------|------|------|
| L-1（🔴 进程/等待句柄泄漏） | ✅ 已修复 | `59583fe`；残留 N-4（PID 竞态）、N-5（复制代码） |
| L-2（🟠 CreateProcessW 悬垂指针） | ✅ 已修复 | `59583fe` |
| L-3（🟠 启动器配置索引错位） | ⚠️ 仍存在 | 即 N-8 |
| PR-1（🟠 新增规则越界） | ⚠️ 仍存在 | 即 N-7 |
| GDI 模糊区域泄漏 | ✅ 已修复 | `6604cd6` |

---

## 六、验证方法附录

1. **单元终结顺序**：核对 `D:\Apps\scoop\apps\Lazarus\4.8\lcl\forms.pp`（finalization `FreeThenNil(Application)`）、`lcl/include/wincontrol.inc`（`TWinControl.Destroy` 先 `DestroyHandle` 后销毁拥有组件；`HandleNeeded` 在 `csDestroying` 下为空操作）；并以最小化 FPC 3.2.2 复现实验实证依赖形状相同的终结顺序（输出见 N-1）。
2. **热键边界值**：`traynard.types.pas` 确认 `HOTKEY_NONE = 0`、`TEST_HOTKEY_ID = 0`。
3. **本地化**：`msgfmt --check-format`、`msgcmp` 实测三个 `.po` 对 `traynard.pot`；定位裸换行位置。
4. **发布流水线**：`.github/workflows/release.yml` 确认 `msgfmt` 逐文件编译且无失败检查。
5. **淡入淡出数学**：按 `ALPHA_BLEND_VALUE_STEP = 15`、`AlphaBlendValue ∈ [0,255]` 手工推演两方向序列。
