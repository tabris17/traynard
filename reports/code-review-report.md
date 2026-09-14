# Traynard 代码审查报告

| 项目 | 内容 |
|------|------|
| 项目 | Traynard（Windows 托盘最小化工具，Lazarus / Free Pascal） |
| 审查日期 | 2026-08-10 |
| 代码规模 | ~10,280 行 Pascal（src/*.pas） |
| 审查方法 | 按模块分组并行人工审查 + 跨模块契约验证；所有 🔴/🟠 级别问题均经逐行复核 |

---

## 一、执行摘要

本次审查覆盖全部源码模块，共确认 **44 个缺陷**，分布如下：

| 严重程度 | 数量 | 说明 |
|----------|------|------|
| 🔴 严重 (Critical) | 1 | 必然发生的内核资源泄漏，长时间运行可耗尽句柄 |
| 🟠 高 (High) | 10 | 释放后使用、崩溃、数据损坏、用户设置被静默覆盖 |
| 🟡 中 (Medium) | 13 | 资源/内存泄漏、契约违反、功能缺陷、配置路径错误 |
| 🟢 低 (Low) | 20 | 潜在越界、未初始化、UI 状态残留、异常吞噬等 |

**最需优先修复的问题：**

1. **启动器进程句柄/等待句柄泄漏**（L-1，Critical）——每次启动进程必然泄漏 1 个进程句柄 + 1 个等待注册。
2. **分组托盘菜单 `Clear` 释放后使用**（WT-1，High）——2 窗口分组收缩为单窗口后再分组，复用已释放的 `FRestoreGroupMenuItem`。
3. **`MinimizeWindow` 双重跟踪**（WT-2，High）——窗口最小化后同时存在于 FDesktop 与 FTray，导致还原后托盘项泄漏、重复最小化失败、幻影托盘图标。
4. **`CreateProcessW` 命令行/工作目录悬垂指针**（L-2，High）——传递给子进程的参数来自已释放内存。
5. **配置集合索引错位**（L-3 / R-1，High）——损坏/重名条目导致删除/修改操作写错配置项（启动器与规则模块均有此问题）。
6. **新增条目时列表越界**（PLE-1 / PR-1，High）——保存新建条目必然触发越界（启动器与规则编辑器均有此问题）。
7. **默认托盘位置设置被静默覆盖并写盘**（PO-1，High）——选“托盘图标”的用户每次打开选项页都被重置为“托盘菜单”。
8. **开机自启注册表值缺少 null 终止符**（SS-2，Medium）——读回时末字符被截断，`FRunAsAdministrator` 被误判为 False。

---

## 二、审查范围与方法

- **方法**：将源码按功能模块分组并行深入审查；针对跨进程/跨模块契约（主程序与 hook DLL 的共享内存 IPC、配置集合与列表的索引一致性、热键槽位回收）进行专项验证。
- **复核原则**：所有 🔴/🟠 级别问题均由审查者逐行读取源码独立确认。`launcher`/`hotkey` 等模块经多轮审查（首轮部分代理因输出超限失败，已重派或由审查者亲自完成）。
- **覆盖模块**（全部）：`window`、`tray`、`launcher`、`hotkey`、`form.hotkey`、`notification`、`session`、`rule`、`page.rules`、`page.options`、`page.desktop`、`page.launcher`、`page.launcher.entries`、`page.tray`、`page.pas`、`page.about/license/logs`、`form.main`、`form.background`、`form.popup`、`helpers`、`helpers.windowlist`、`highlight`、`task`、`settings`、`storage`、`i18n`、`strings`、`types`、`lib`（hook DLL）。

---

## 三、缺陷详情

### 🔴 严重 (Critical)

#### L-1 · 启动器进程句柄与等待句柄泄漏
- **位置**：`traynard.launcher.pas:452-479`（`Launch`）、`273-277`（`ProcessExitEvent`）、`588-591`（`WaitProcess`）
- **类别**：资源泄漏（内核句柄）
- **描述**：`Launch` 经 `RegisterWaitForSingleObject` 注册等待（`WaitHandle` 为局部变量，成功路径从不保存），且 `ProcessInfo.hProcess` 未存入 `TProcess` 记录（仅含 Name/PID/CreationTime/Application/Arguments）。进程退出时 `WaitProcess` 仅 `QueueAsyncCall`，`ProcessExitEvent` 仅 `FProcesses.Remove`——两者均**未调用 `UnregisterWait` 或 `CloseHandle`**。
- **失败场景**：每次成功启动进程必然泄漏 1 个进程句柄和 1 个等待注册（`WT_EXECUTEONLYONCE` 不自动注销）。长期使用启动器导致句柄持续累积，最终可能耗尽句柄表/线程池资源。
- **修复建议**：在 `TProcess`（或并行字典）保存 `WaitHandle` 与 `hProcess`；`ProcessExitEvent` 中先 `UnregisterWait(WaitHandle)` 再 `CloseHandle(hProcess)`。

---

### 🟠 高 (High)

#### WT-1 · 分组托盘菜单 `Clear` 释放后使用 `FRestoreGroupMenuItem`
- **位置**：`traynard.tray.pas:404`（`RemoveWindow` 的 `Clear`）、`350`（`AddWindow` 复用）、`318-326`（构造）
- **类别**：释放后使用（use-after-free）
- **描述**：`FRestoreGroupMenuItem` 在构造时创建（行 323），分组时经 `Add(FRestoreGroupMenuItem)` 加入为子项（行 350）。`RemoveWindow` 在 `Count = 3`（2 窗口分组收缩为单窗口）时调用 `Clear`（行 404）。LCL 的 `TMenuItem.Clear` 对每个子项调用 `Free`，故 `FRestoreGroupMenuItem` 被释放，但字段从未置 nil。下次 `AddWindow` 再次分组时行 350 `Add(FRestoreGroupMenuItem)` 复用已释放对象。
- **失败场景**：同一应用的 2 窗口最小化（分组）→ 还原其中一个（收缩为单窗口，`Clear` 释放 `FRestoreGroupMenuItem`）→ 再最小化同应用另一窗口（重新分组）→ `Add` 悬垂指针 → 访问冲突或内存损坏。
- **修复建议**：用逐项 `Delete`（不释放）替代 `Clear`，或 `Clear` 后重建 `FRestoreGroupMenuItem`，或对单个项使用 `Remove`。

#### WT-2 · `MinimizeWindow` 将窗口同时留在 FDesktop 与 FTray（双重跟踪）
- **位置**：`traynard.window.pas:1213-1233`（`MinimizeWindow`）、`788-809`（`AddWindow` 还原分支）
- **类别**：逻辑错误 / 状态不一致
- **描述**：`MinimizeWindow` 调用 `FTray.FWindows.Add(...)`（行 1227）但**不从 `FDesktop.FWindows` 移除**。于是 HWND 同时存在于两集合。`AddWindow`（窗口再次出现时）行 788 `FDesktop.FWindows.TryGetValue` 先命中“已存在于桌面”分支并 `Exit(False)`，使行 802-809 的“从托盘还原”分支**永远不可达**。
- **失败场景**：还原后托盘项不清理（图标持续显示，需二次点击经“已可见”错误路径消除）；再次最小化时 `FTray.FWindows.Add` 因重复键抛异常（被 `TryMinimizeWindow` 吞掉，静默失败）；窗口在最小化期间被销毁则留下永久幻影托盘图标。
- **修复建议**：`MinimizeWindow` 中于加入 FTray 前 `FDesktop.FPONotifyObservers(Self, ooDeleteItem, ...); FDesktop.FWindows.Remove(Window.Handle);`。

#### L-2 · `CreateProcessW` 命令行/工作目录悬垂指针
- **位置**：`traynard.launcher.pas:435`、`437`
- **类别**：释放后使用（悬垂指针）
- **描述**：`CmdLine := PWideChar(UnicodeString(Entry.Arguments))` 产生的临时 `UnicodeString` 在赋值语句末尾引用计数归零被释放，`CmdLine`/`CurrDir` 悬垂，待行 439 `CreateProcessW` 读取时已指向已释放内存。（行 439 内联的 `PWideChar(UnicodeString(Entry.Application))` 临时对象存活至整条语句结束，故安全。）
- **失败场景**：传给子进程的命令行/工作目录来自已释放堆内存；`CurrDir` 的转换甚至可能复用 `CmdLine` 刚释放的缓冲区导致两者互换或损坏。当前“碰巧”可用仅因堆未及时复用。
- **修复建议**：改用存活的局部 `UnicodeString` 变量持有参数与工作目录，再取 `PWideChar` 传入 `CreateProcessW`。

#### L-3 · 启动器配置集合与条目列表索引错位
- **位置**：`traynard.launcher.pas:494-519`（`Load`）、`537-554`（`RemoveEntry`）、`556-586`（`UpdateEntry`）
- **类别**：数据损坏
- **描述**：`Load` 中 `FConfigEntries`（TOML 数组，含全部条目）与 `FEntryList`（仅成功加载、非重名条目）按下标对应。当 `Entry.Load` 抛异常（行 511 `Continue`）或 `FEntryMap.TryAdd` 返回 False（重名，行 515）时，`FEntryList` 不追加而 `FConfigEntries` 仍保留该项——两集合错位。`RemoveEntry`/`UpdateEntry` 用**同一 `EntryIndex`** 访问二者（行 543/549、563/578），读取/修改/删除错误的 TOML 配置项。
- **失败场景**：配置存在加载失败或重名条目时，其后所有条目的删除/修改都作用到错误配置项，逐步损坏保存的配置。
- **修复建议**：每条目旁记录其配置项索引（或跳过时从 `FConfigEntries` 同步移除），保证两集合索引对齐。

#### R-1 · 规则配置集合与列表索引错位（同 L-3 模式）
- **位置**：`traynard.rule.pas:412-424`（`Load`）、`441-458`（`RemoveRule`）、`460-490`（`UpdateRule`）
- **类别**：数据损坏
- **描述**：`TRules.Load` 中 `FConfigRules` 与 `FRuleList` 同样在 `Rule.Load` 抛异常（行 414-418 `Continue`）或 `FRuleMap.TryAdd` 返回 False（重名，行 419）时错位。`RemoveRule`/`UpdateRule` 用同一 `RuleIndex` 同时索引 `FRuleList[RuleIndex]` 与 `FConfigRules.Items[RuleIndex]`（行 447/453、467/482）。
- **失败场景**：规则文件存在加载失败或重名规则时，删除/修改规则写错配置项，损坏 rules 配置。
- **修复建议**：同 L-3——保持两集合索引对齐，或跳过时同步移除配置项。

#### PLE-1 · 新增启动器条目时 ListBox 越界
- **位置**：`traynard.page.launcher.entries.pas:391`
- **类别**：数组越界
- **描述**：`esNew` 分支 `EntryIndex := Launcher.AddEntry(Entry)`（行 379）返回新索引 = 原数量 N。但进入 `esNew` 及随后 `EditState := esOpen` 均未向 ListBox 添加项，ListBox 仍为 N 项（有效下标 0..N-1）。行 391 `ListBoxEntries.Items[EntryIndex]` 即 `Items[N]` 越界。行 356-394 无 try/except（行 328-354 仅捕获 `EFieldInvalid`），异常传播至 LCL。
- **失败场景**：每次保存新建启动器条目都触发越界——LCL 抛“列表下标越界”弹错误对话框；即便某些版本静默忽略，新条目虽已写盘却不显示于列表，直至页面重载。
- **修复建议**：`esNew` 分支用 `ListBoxEntries.Items.Add(Entry.Name)`，就地更新 `Items[EntryIndex]` 仅用于 `esOpen`。

#### PR-1 · 新增规则时 ListBox 越界（同 PLE-1 模式）
- **位置**：`traynard.page.rules.pas:509`（`ActionSaveExecute` 的 esNew 分支 494-507）
- **类别**：数组越界
- **描述**：`esNew` 分支 `RuleIndex := Rules.AddRule(Rule)`（行 497）返回 N，但 ListBox 未添加项仍为 N 项；行 509 `ListBoxRules.Items[RuleIndex]` 越界。
- **失败场景**：每次保存新建规则触发越界/错误对话框，或新规则不显示于列表直至重载。
- **修复建议**：`esNew` 分支用 `ListBoxRules.Items.Add(Rule.Name)`。

#### A6-1 · `LoadFromExe` 泄漏 `ExtractIcon` 返回的 HICON
- **位置**：`traynard.helpers.pas:368-376`（`TBitmapHelper.LoadFromExe`）
- **类别**：GDI 资源泄漏
- **描述**：`ExtractIcon(hInstance, PChar(ExePath), 0)` 返回的 HICON 须由调用方 `DestroyIcon`。`LoadFromHIcon`（行 339-366）仅 `DrawIconEx`（非破坏性）从不销毁；`LoadFromExe` 返回前也未 `DestroyIcon`。被 `traynard.tray.pas:355`（分组托盘图标创建）调用。
- **失败场景**：每次应用分组转为显示 exe 图标泄漏 1 个 HICON。长期托盘会话频繁最小化/还原累积 GDI 句柄，可能耗尽进程 GDI 配额（~10,000）导致渲染失败。
- **修复建议**：`LoadFromHIcon` 后 `DestroyIcon(IconHandle)`（建议 `try/finally`）。

#### PLE-2 · 保存校验失败时启动器编辑器数据被清空
- **位置**：`traynard.page.launcher.entries.pas:483-498`（`ActionCloseExecute`）
- **类别**：数据丢失
- **描述**：用户点“是(保存)”执行 `ActionSaveExecute`（行 491）。若校验失败，`ActionSaveExecute` 弹提示后 `Exit`（行 352），`Unsaved` 仍为 True。随后**穿透**到行 497 `EditState := esNone`，`SetEditState(esNone)` 调 `ClearEditor`（行 291）销毁用户已输入数据。
- **失败场景**：用户编辑后关闭选“保存”，某字段校验未通过——本应让用户修正，却被直接清空，全部输入丢失。
- **修复建议**：IDYES 分支后 `if Unsaved then Exit;`（规则编辑器 `page.rules.pas:437` 已正确如此处理，可参照）。

#### PO-1 · 默认托盘位置设置被静默覆盖并持久化
- **位置**：`traynard.page.options.pas:380-384`（`Initialize`）、`402-406`（`AddListener`）、`140-143`（`DefaultTrayPositionChange`）、`539-547`（`DefaultTrayPositionChanged`）；`traynard.settings.pas:227-234`（`SetDefaultTrayPosition`）
- **类别**：设置被覆盖 / 数据丢失
- **描述**：`TPageOptions.AddListener`（行 404）在注册前**立即调用** `Listener(Settings)`。`Initialize` 行 380 注册 `DefaultTrayPositionChanged`，此时行 383-384 的 `RadioButtonTrayMenu.Tag := Ord(tpMenu)` / `RadioButtonTrayIcon.Tag := Ord(tpIcon)` **尚未执行**，两按钮 `Tag` 仍为 0。监听器据 `Settings.DefaultTrayPosition` 置 `RadioButtonTrayIcon.Checked := True`（若用户保存的是 tpIcon），触发 `DefaultTrayPositionChange` 读 `Sender.Tag = 0` → `tpMenu` → `Settings.DefaultTrayPosition := tpMenu`。`SetDefaultTrayPosition`（行 231-232）随即写配置并 `SaveConfig` **存盘**。
- **失败场景**：把“默认托盘位置”设为“托盘图标”的用户，一旦选项页初始化即被静默重置为“托盘菜单”并写盘，永久丢失。
- **修复建议**：将行 383-384 的 `Tag` 赋值移到行 380 `AddListener` 调用之前。

---

### 🟡 中 (Medium)

#### SS-1 · `GetAppConfigDir` 回退分支缺少尾部分隔符，路径错误合并
- **位置**：`traynard.storage.pas:251-255`、`259`、`271`
- **类别**：路径处理
- **描述**：行 251 主路径用 `IncludeTrailingPathDelimiter(...)`，但回退分支行 253 `FAppDataDir := GetAppConfigDir(False)` 未加尾部分隔符。随后行 255/259/271 用 `FAppDataDir + CONFIG_DIR` / `+ LANGUAGES_DIR` 裸字符串拼接，产生 `...\Roamingconfig`、`...\Roaminglanguages` 等合并名。（FPC 的 `GetAppConfigDir` 标准行为不返回尾部分隔符；行 251 与 253 的不对称表明作者本意要求 FAppDataDir 带尾部分隔符。）
- **失败场景**：独立版（standalone，单 exe 无 `data` 目录，配置写入 AppData）必然走回退分支，配置目录与语言目录路径错误合并，`.mo` 翻译文件无法被找到（仅内置/母语可用），配置存入非标准合并名目录。
- **修复建议**：行 253 改 `FAppDataDir := IncludeTrailingPathDelimiter(GetAppConfigDir(False))`，或行 255/259/271 改用 `ConcatPaths`。

#### SS-2 · 开机自启注册表 REG_SZ 值缺少 null 终止符
- **位置**：`traynard.settings.pas:463`（写）、`437-444`（读）
- **类别**：Win32 API 用法 / 数据截断
- **描述**：行 463 `RegSetValueExW(..., Length(CommandLine) * SizeOf(WideChar))` 的 `cbData` 不含终止 null（REG_SZ 应含）。读回代码行 443 `SetLength(CommandLine, DataSize div SizeOf(WideChar) - 1)` 假定 null 存在而减 1。写不含 null、读假定有 null，二者不一致。
- **失败场景**：读回时末字符被截断。开机自启命令行末尾为 `--run-task`，截断后变 `--run-tas`，使行 444 `EndsStr('--run-task', ...)` 为 False，`FRunAsAdministrator` 被误判为 False，导致“以管理员身份运行”的自启设置在下次启动时丢失/被改写为 `--silent` 形式。
- **修复建议**：写时 `cbData` 传 `(Length(CommandLine) + 1) * SizeOf(WideChar)`。

#### M1 · i18n 共享内存大小计算错误，第 3 个系统菜单项不被翻译
- **位置**：`traynard.types.pas:236`（常量）、`traynard.i18n.pas:271`（生产者）、`traynard.lib.lpr:56-74`（消费者 `Translate`）
- **类别**：逻辑错误 / 缓冲区越界
- **描述**：`SYSTEM_MENU_LANG_DATA_MIN_SIZE = SizeOf(DWORD) * SizeOf(SYSTEM_MENU_LANG_DATA_ITEMS)`。`SYSTEM_MENU_LANG_DATA_ITEMS` 是**集合**，`SizeOf(集合) = 1`（位域），而作者意图是条目数 = 3。故常量为 4 而非 12，文件映射比实际小 8 字节。生产者无条件写入 3 个 DWORD 前缀 + 文本，溢出映射末尾 8 字节（恰落同页未崩溃）。消费者以偏小的 `DataSize` 做边界检查，累加到第 3 项时 `Count = 12 + Σ` 必然 `> DataSize` 而 `Break`，**第 3 项 `smiTopmost`（“始终置顶”）未被翻译**。
- **失败场景**：选非母语语言时，目标窗口系统菜单中“始终置顶”保持默认文本未翻译；生产者另有 8 字节越界写入隐患。
- **修复建议**：常量按条目数计算（如 `SizeOf(DWORD) * 3`）。

#### M3 · 损坏的 .mo 文件导致语言下拉框出现空白条目
- **位置**：`traynard.i18n.pas:242-253`（`GetAvailableLanguages`）
- **类别**：逻辑错误
- **描述**：循环中 `SetLength(FLanguageList, Count + 1)` 先扩容，随后 `try ... except Inc(Count); Continue; end`。若 `TMOFile.Create` 或 `ParseLanguage` 抛异常，已分配槽位未填充（默认空 `TLanguage`），`Inc(Count)` 又越过该槽，留下永久空白。
- **失败场景**：`locale` 目录存在损坏 `.mo` 文件时，语言下拉框出现空白选项。
- **修复建议**：异常分支移除 `Inc(Count)`（使下轮复用槽位），或异常前回退 `SetLength`。

#### A6-2 · 托盘菜单动态项 `Delete` 不释放对象
- **位置**：`traynard.form.background.pas:266-288`（`SetLaunchMenuItems`，行 274-275）
- **类别**：内存泄漏
- **描述**：`MenuItemLaunch.Delete(ItemIndex)` 在 LCL 仅从父菜单项列表移除指针，**不 `Free`**。项由 `TMenuItem.Create(Self)` 创建归 `FormBackground` 所有，成为孤儿，仅析构时释放。每次托盘菜单弹出重建都累积 N 个孤儿。
- **修复建议**：改用 `MenuItemLaunch.Items[ItemIndex].Free`。

#### L-4 · 启动器 `Load` 循环中 `Entry` 未重置，可选字段泄漏
- **位置**：`traynard.launcher.pas:494-519`（`Load`）、`180-189`（`TLaunchEntry.Load`）
- **类别**：数据泄漏
- **描述**：`Entry` 为循环外声明的单一局部记录复用。`TLaunchEntry.Load` 仅在 `TryGetValue` 成功时赋值 `Arguments`/`WorkingDirectory`（行 186-189）；某条目省略这些可选键则继承上一条目值，存入 `FEntryMap`。
- **失败场景**：条目 A 设参数 `--foo`，条目 B 未设参数——B 继承 A 的 `--foo`，启动 B 时错误传入 A 的参数。
- **修复建议**：循环体首行 `Entry := Default(TEntry);`。

#### R-2 · 规则 `Load` 循环中 `Rule` 未重置，可选字段泄漏（同 L-4）
- **位置**：`traynard.rule.pas:494-519`（`Load`）、`167-219`（`TRule.Load`）
- **类别**：数据泄漏
- **描述**：`Rule` 单一局部记录复用。`TRule.Load` 仅在 `TryGetValue` 成功时赋值 `WindowTitle`/`WindowClass`/`AppPath`（行 174-199）；省略这些键的规则继承上一规则的值。
- **失败场景**：规则 A 设了 WindowTitle，规则 B 省略——B 继承 A 的 WindowTitle，匹配错误窗口。
- **修复建议**：循环体首行 `Rule := Default(TRule);`。

#### R-3 · 规则 schema 迁移用魔术数 `TWindowAction(4)` 与注释“waExisting”(=5) 不符
- **位置**：`traynard.rule.pas:212-216`
- **类别**：逻辑错误 / 迁移缺陷
- **描述**：`if Schema < RULE_SCHEMA_V1 then begin { fix waExisting issue } Exclude(TriggerOn, TWindowAction(4)); end;`。`TWindowAction(4)` = `waDeactivated`，而注释所指 `waExisting` 的序数值为 5（`TWindowAction = (waCreation, waChange, waMinimizing, waHotkey, waDeactivated, waExisting)`）。迁移实际剥离的是 `waDeactivated`，与注释陈述的意图不符。
- **失败场景**：所有旧 schema（无 schema 键 = 0）规则在加载时其 `waDeactivated` 触发被剥离，导致升级后旧规则不再于窗口失活时触发；若原意为清除 `waExisting` 则未生效。
- **修复建议**：使用命名常量明确意图——若为 `waExisting` 应为 `TWindowAction(5)`/`waExisting`，若为 `waDeactivated` 应直接写 `waDeactivated` 并更正注释。

#### L-5 · `TryLaunch(Name)` 对不存在的名称抛异常而非返回 False
- **位置**：`traynard.launcher.pas:361-364`
- **类别**：契约违反
- **描述**：`Result := TryLaunch(FEntryMap[Name]);`。`FEntryMap[Name]` 在键不存在时抛异常，且发生在内部 `TryLaunch(Entry)` 的 try/except（行 353-358）**之前**。“Try”前缀暗示不应抛异常。
- **修复建议**：先 `FEntryMap.TryGetValue(Name, Entry)`，未找到则 `Exit(False)`。

#### L-6 · `WaitProcess` 回调在析构后访问 `FSelf`
- **位置**：`traynard.launcher.pas:307-321`（析构）、`588-591`（`WaitProcess`）
- **类别**：释放后使用 / 竞态
- **描述**：析构未将 `FSelf` 置 nil，也未注销未完成等待（见 L-1）。线程池等待若在 `FreeAndNil(Launcher)` 后触发，`WaitProcess` 解引用已释放 `FSelf`；且 `Application.QueueAsyncCall` 可能在 `Application` 已终结后被调用。
- **修复建议**：析构中置 `FSelf := nil`，`WaitProcess` 开头 `if FSelf = nil then Exit;`；注销所有未完成等待。

#### PL-1 · 启动器页面 `LaunchMenu` 动态项 `Delete` 不释放（同 A6-2 模式）
- **位置**：`traynard.page.launcher.pas:131-135`（`SetLaunchMenuItems`）
- **类别**：内存泄漏
- **描述**：`LaunchMenu.Items.Delete(ItemIndex)` 不释放 `TMenuItem`（`Create(Self)`，归页面所有），每次 `PageActivate` 重建累积孤儿。
- **修复建议**：改用 `LaunchMenu.Items[ItemIndex].Free`。

#### PL-2 · `UpdateProcessList` 泄漏 `ExtractIcon` 返回的 HICON（同 A6-1 根因）
- **位置**：`traynard.page.launcher.pas:117`
- **类别**：GDI 资源泄漏
- **描述**：`IconList.AddIcon(ExtractIcon(...), 0)` —— `AddIcon` 仅复制数据不接管 HICON；返回的 HICON 从未 `DestroyIcon`。每次 `UpdateProcessList` 按进程数泄漏。亦未检查 `ExtractIcon` 返回 0/1（失败）。
- **修复建议**：局部变量保存 HICON，`AddIcon` 后 `try/finally DestroyIcon`。

#### PL-3 · `Finalize` 未注销进程观察者
- **位置**：`traynard.page.launcher.pas:176-180`（`Finalize`）、`49-55`、`72-76`
- **类别**：悬垂引用 / 释放后使用
- **描述**：观察者在 `PageActivate`（行 53）注册、`PageDeactivate`（行 74）注销。`Finalize` 仅移除 Settings 监听器，**未** `Launcher.Processes.FPODetachObserver(Self)`。页面在活动状态下被销毁（如关窗未触发 `PageDeactivate`）时，全局 `Launcher.Processes` 持有悬垂观察者，下次进程增删调用 `FPOObservedChanged` 即 AV。
- **修复建议**：`Finalize` 中 `inherited` 前调用 `Launcher.Processes.FPODetachObserver(Self)`。

---

### 🟢 低 (Low)

#### M2 · `_T` 返回指向临时 `UnicodeString` 的 `PWideChar`（依赖内联）
- **位置**：`traynard.lib.lpr:15-18`
- **类别**：悬垂指针（条件性）
- **描述**：`Result := PWideChar(UTF8Decode(Str))` 返回指向临时 `UnicodeString` 缓冲区的指针，仅函数被内联时存活至调用方语句结束；若未内联则函数返回时临时对象释放，`PWideChar` 悬垂。
- **修复建议**：由调用方持有 `UnicodeString` 局部变量再取 `PWideChar`，或返回 `UnicodeString`。

#### A6-3 · `CreateDIBSection` 返回值未检查
- **位置**：`traynard.highlight.pas:114-117`（`TFrameDrawer.Draw`）
- **类别**：未初始化解引用
- **描述**：`HBmp := CreateDIBSection(...)` 后未检查 `HBmp = 0`。失败时（内存压力或 `Rect.Width/Height` 为 0/负，如 DWM 对最小化窗口返回退化矩形），`Bits`（局部未初始化）为垃圾，`FillChar(Bits^, ...)` 解引用致 AV。亦无 try/finally 清理 DC。
- **修复建议**：`if HBmp = 0 then begin 释放 DC; Exit; end;`，并以 try/finally 包裹 GDI 资源。

#### A6-4 · `WM_CREATE` 处理未设置返回值
- **位置**：`traynard.highlight.pas:304-309`（`WindowProc`）
- **类别**：未定义行为
- **描述**：`WM_CREATE` 分支仅设置 `GWLP_USERDATA` 与 `SetTimer`，未显式 `Result := 0`。`LRESULT` 隐式 `Result` 未初始化；返回 -1 会中止窗口创建。
- **修复建议**：分支末尾 `Result := 0;`。

#### A6-5 · 任务计划临时 XML 文件未删除
- **位置**：`traynard.task.pas:94`、`117`
- **类别**：资源泄漏（临时文件）
- **描述**：`SysUtils.GetTempFileName` 创建 0 字节文件，写入后被 `schtasks.exe` 消费，全程无 `DeleteFile(XMLPath)`。每次创建管理员任务遗留一个临时 XML。
- **修复建议**：`Exec` 后 `SysUtils.DeleteFile(XMLPath)`（try/finally）。

#### A6-6 · `SetCurrentPageIndex` 未对新页面做 nil 检查
- **位置**：`traynard.form.main.pas:360`
- **类别**：空指针解引用（潜在）
- **描述**：方法对旧页面做 `Assigned` 检查，但更新 `FCurrentPageIndex` 后无条件访问新页面 `Visible`/`FullFrame`。若 `AValue = piNone`（页面实例为 nil）或创建失败即 nil 解引用。当前无内部调用者传入 `piNone`。
- **修复建议**：取新页面后 `if not Assigned(Page) then Exit;`。

#### A6-7 · `Initialize/Finalize` 包裹无 try/finally，异常时 `ExceptProc` 不恢复
- **位置**：`traynard.task.pas:85-118`（及 `121-127`、`129-134`）
- **类别**：全局状态未恢复
- **描述**：`Initialize` 保存原 `ExceptProc` 并安装 `@HandleException`，`Finalize` 恢复。二者间 `AssertRunAsAdministrator`/`Exec` 可抛 `ERuntimeError`；若抛异常则 `Finalize` 被跳过，`ExceptProc` 永久被替换。当前因调用后立即 `Exit` 终止进程而无害，但模式脆弱。
- **修复建议**：以 `try ... finally Finalize; end;` 包裹。

#### A6-8 · `LoadFromExe` 在 `LoadFromHIcon` 失败时仍返回 True
- **位置**：`traynard.helpers.pas:375`
- **类别**：错误返回值
- **描述**：`LoadFromHIcon` 在 `CreateDIBSection` 失败时跳过 `Self.Handle := MemBmp`，位图未变；`LoadFromExe` 却无条件 `Result := True`。调用方误以为成功，使用空白/陈旧位图。
- **修复建议**：令 `LoadFromHIcon` 返回布尔成功值并向上传递。

#### HK-1 · `Hotkey[HotkeyID]` 公开属性无边界检查
- **位置**：`traynard.hotkey.pas:100-103`（`GetHotkey`）
- **类别**：数组越界（潜在）
- **描述**：`Result := FHotkeys[HotkeyID]` 无边界检查。当前唯一调用方传入有效 ID，故安全；公开属性未设防。
- **修复建议**：索引前 `if (HotkeyID < 0) or (HotkeyID >= Length(FHotkeys)) then Exit(Default(THotkeyInfo));`。

#### HK-2 · `CenterPanelControls` 在空面板时解引用未初始化 `Control`
- **位置**：`traynard.form.hotkey.pas:150-164`
- **类别**：空指针解引用（潜在）
- **描述**：若 `Panel.ControlCount = 0`，`LastIndex = -1`，`for ... downto 0` 不执行，局部 `Control` 未赋值；行 163 `Control.Left := ...` 解引用垃圾。当前两调用方均有子控件。
- **修复建议**：开头 `if Panel.ControlCount = 0 then Exit;`。

#### HK-3 · 热键槽位不回收，数组与 Win32 ID 单调增长
- **位置**：`traynard.hotkey.pas:267-284`（`Register`）、`286-296`（`Unregister`）
- **类别**：资源/内存缓慢增长
- **描述**：`Register` 总在 `Length(FHotkeys)` 处追加；`Unregister` 仅置 `hsNone` 不回收/缩容。每次规则/启动器条目更新（remove+add）永久泄漏一个 `THotkeyInfo` 槽位并推进 Win32 hotkey ID。Win32 热键本身已正确注销，仅进程内数组缓慢增长。
- **修复建议**：`Unregister` 时若尾部空闲则缩容，或 `Register` 复用空闲槽位。

#### PLE-3 · `TestHotkey` 冲突热键标红后未在非冲突时重置颜色
- **位置**：`traynard.page.launcher.entries.pas:214-224`、`466`
- **类别**：UI 状态未重置
- **描述**：冲突热键将 `LabelHotkey.Font.Color` 置 `clRed`；查看非冲突热键条目时仅更新 Caption，不重置 `Font.Color`/`ShowHint`，红色残留。`Entry.Hotkey.Value = 0` 分支（行 466）同样未重置。
- **修复建议**：`TestHotkey` 增加 `else` 分支重置 `Font.Color := clDefault`、`ShowHint := False`。

#### PR-2 · 规则编辑器 `ActionOpenExecute` 无热键分支未重置颜色（同 PLE-3 模式）
- **位置**：`traynard.page.rules.pas:382-386`
- **类别**：UI 状态未重置
- **描述**：`Rule.Hotkey.Value = 0` 分支仅设 `LabelHotkey.Caption`，未重置 `Font.Color`/`ShowHint`。若上一规则热键冲突（红色），打开无热键规则时标签残留红色与提示。
- **修复建议**：该分支调用 `RestoreLabelHotkey`（已存在，重置颜色与提示）。

#### PO-2 · 桌面页复制到剪贴板的 HTML 数据行用 `<th>` 而非 `<td>`
- **位置**：`traynard.page.desktop.pas:284`
- **类别**：逻辑错误（HTML 语义）
- **描述**：`ActionCopyEntireRowExecute` 第二个 `<tr>`（数据行）中 Handle 值用 `'<th>{{Handle}}</th>'`，其余数据单元均用 `<td>`。粘贴到解析 HTML 的应用时 Handle 被渲染为表头单元。
- **修复建议**：改为 `'<td>{{Handle}}</td>'`。

#### PO-3 · `IfThen<string>` 无条件求值可能越界
- **位置**：`traynard.page.options.pas:201`
- **类别**：数组越界（潜在）
- **描述**：`specialize IfThen<string>(SelectedIndex >= 0, I18n.AvailableLanguages[SelectedIndex].Code, '')`。Pascal 函数参数调用前全部求值，`AvailableLanguages[SelectedIndex]` 即便 `SelectedIndex = -1` 也会被访问。当前因 ComboBox 为 `csDropDownList` 且 `ItemIndex` 始终有效而不触发。
- **修复建议**：改用 `if/else` 语句。

#### SS-3 · `TConfigHelper.Next` 移除非表值时不释放
- **位置**：`traynard.storage.pas:143-155`（行 151）
- **类别**：内存泄漏
- **描述**：若某键已存在但为非 `TTOMLTable` 值（配置损坏/类型混杂），`Items.Remove(AKey)` 从集合移除但**不释放**对象，孤儿 `TTOMLValue` 泄漏（对比 `SetValue` 行 175-176 显式 `TheValue.Free`）。
- **修复建议**：行 151 移除前 `NextValue.Free`。

#### SS-4 · `TTrayPosition` 由未校验的配置整数强转
- **位置**：`traynard.settings.pas:397`
- **类别**：类型/范围
- **描述**：`TTrayPosition(FConfig.GetInteger(...))`，若存储整数超出 0..1（损坏/手改配置），产生非法枚举值，后续集合/case 操作未定义行为。
- **修复建议**：强转前校验整数在 `Ord(Low)..Ord(High)` 内，否则回退默认值。

#### SS-5 · `HighlightTopmostThickness` 经未校验 Byte 强转截断
- **位置**：`traynard.settings.pas:379`
- **类别**：截断
- **描述**：`Byte(FConfig.GetInteger(...))` 静默截断超出 0..255 的值（如 300 → 44），损坏配置产生意外厚度且无报错。
- **修复建议**：强转前将整数钳制到 0..255。

#### R-4 · 规则匹配中正则异常未捕获
- **位置**：`traynard.rule.pas:130-146`（`Equal`）、`363-373`（`Find`）、`375-380`（`Match`）
- **类别**：异常处理
- **描述**：`rtcRegexMatch` 分支 `RegEx.Expression := RuleText.Text; RegEx.Exec`，若模式非法，`TRegExpr` 抛 `ERegExpr`。`Equal`/`Find`/`Match` 无 try/except。规则编辑器不校验正则合法性，故用户可保存含非法正则的规则。
- **失败场景**：含非法正则的规则在窗口匹配时抛异常，是否被上层（`window.pas` 调用方）捕获决定其影响——未捕获则干扰窗口处理流程。
- **修复建议**：`Equal` 的正则分支以 try/except 包裹，非法正则视为不匹配。

#### WT-4 · `GetLastWindow` 未检查列表为空即取 `.Last`
- **位置**：`traynard.window.pas:437-440`
- **类别**：空列表访问
- **描述**：`Result := FWindows[FOrderedWindowList.Last]`，`FOrderedWindowList` 为空时 `.Last` 抛 `EListError`。托盘为空时经 `TryRestoreLastWindow → FTray.LastWindow → GetLastWindow` 调用，仅靠外层 try/except 兜底，以异常流处理正常空场景。
- **修复建议**：`if FOrderedWindowList.Count = 0 then Exit(nil)`，调用方检查 nil。

#### WT-5 · 空 except 吞掉 `InstallHook` 失败，状态不一致
- **位置**：`traynard.window.pas:1128-1132`（及 `SetSystemMenuItems` 约 555-566）
- **类别**：异常吞噬
- **描述**：行 1129 `SystemMenuItems := Settings.SystemMenuItems` 调用 setter，setter 先置 `FSystemMenuItems := AValue` 再 `InstallHook`。`InstallHook` 抛异常被行 1131 空 except 吞掉，结果 `FSystemMenuItems` 非空而 `FHookInstalled` 为 False。后续以同值调用 setter 因 `if FSystemMenuItems = AValue then Exit` 提前返回，钩子永不重试，直至用户改设置。
- **修复建议**：记录或上抛异常，或失败时回退 `FSystemMenuItems := []`。

---

## 四、按模块统计

| 模块 | 严重 | 高 | 中 | 低 | 小计 |
|------|------|----|----|----|------|
| `traynard.launcher.pas` | 1 | 2 | 3 | 0 | 6 |
| `traynard.window.pas` | 0 | 1 | 0 | 2 | 3 |
| `traynard.tray.pas` | 0 | 1 | 0 | 0 | 1 |
| `traynard.page.launcher.entries.pas` | 0 | 2 | 0 | 1 | 3 |
| `traynard.page.rules.pas` | 0 | 1 | 0 | 1 | 2 |
| `traynard.rule.pas` | 0 | 1 | 2 | 1 | 4 |
| `traynard.page.launcher.pas` | 0 | 0 | 3 | 0 | 3 |
| `traynard.page.options.pas` | 0 | 1 | 0 | 1 | 2 |
| `traynard.settings.pas` | 0 | 0 | 1 | 2 | 3 |
| `traynard.storage.pas` | 0 | 0 | 1 | 1 | 2 |
| `traynard.helpers.pas` | 0 | 1 | 0 | 1 | 2 |
| `traynard.highlight.pas` | 0 | 0 | 0 | 2 | 2 |
| `traynard.task.pas` | 0 | 0 | 0 | 2 | 2 |
| `traynard.hotkey.pas` / `form.hotkey.pas` | 0 | 0 | 0 | 3 | 3 |
| `traynard.form.background.pas` | 0 | 0 | 1 | 0 | 1 |
| `traynard.form.main.pas` | 0 | 0 | 0 | 1 | 1 |
| `traynard.i18n.pas` | 0 | 0 | 1 | 0 | 1 |
| `traynard.types.pas`（跨模块） | 0 | 0 | 1 | 0 | 1 |
| `traynard.lib.lpr`（hook DLL） | 0 | 0 | 0 | 1 | 1 |
| `traynard.page.desktop.pas` | 0 | 0 | 0 | 1 | 1 |
| **合计** | **1** | **10** | **13** | **20** | **44** |

> 说明：`PLE-3`/`PR-2`（同模式 UI 残留）、`L-3`/`R-1`（同模式索引错位）、`PLE-1`/`PR-1`（同模式越界）、`L-4`/`R-2`（同模式字段泄漏）、`A6-1`/`PL-2`（同根因 HICON 泄漏）、`A6-2`/`PL-1`（同模式 TMenuItem 泄漏）均为同一缺陷模式在不同模块的重复出现，已在各自条目中互相引用。统计按首次出现的主要位置归类。

---

## 五、修复优先级建议

1. **立即修复（数据完整性 / 必然崩溃 / 释放后使用）**：L-1、WT-1、WT-2、L-2、L-3、R-1、PLE-1、PR-1、PO-1
2. **尽快修复（资源泄漏持续累积 / 设置损坏）**：A6-1、PL-2、PL-1、A6-2、SS-2、SS-1、M1、L-6、PL-3
3. **择机修复（数据泄漏 / 契约 / 迁移）**：M3、L-4、R-2、L-5、R-3、A6-3、A6-4、HK-3、R-4
4. **加固 / 清理（潜在或外观）**：其余低危项（M2、A6-5、A6-6、A6-7、A6-8、HK-1、HK-2、PLE-3、PR-2、PO-2、PO-3、SS-3、SS-4、SS-5、WT-4、WT-5）

---

## 六、共性模式与系统性建议

本次审查发现多类**跨模块重复出现的缺陷模式**，建议从机制层面统一修复，而非逐点打补丁：

1. **配置集合与运行时列表索引错位**（L-3、R-1）：`Load` 跳过失败/重名条目时配置数组与列表不同步，而 `Remove`/`Update` 假定二者同下标。建议统一为“每条目记录其配置项索引”或在跳过时同步移除配置项。
2. **`esNew` 保存时列表越界**（PLE-1、PR-1）：保存新建条目用 `Items[Index]` 而非 `Items.Add`。建议抽取通用“条目编辑器”基类统一保存逻辑。
3. **`ExtractIcon` HICON 未销毁**（A6-1、PL-2）：两处独立调用均遗漏 `DestroyIcon`。建议封装统一的“从 exe 加载图标到位图/图像列表”工具方法并内建 `DestroyIcon`。
4. **`TMenuItem.Delete` 不释放**（A6-2、PL-1）：动态菜单重建普遍误用 `Delete`。建议统一改用 `Items[i].Free`。
5. **可选字段未重置导致跨条目泄漏**（L-4、R-2）：复用记录 + 条件赋值。建议 `Load` 循环体首行 `Default(T)` 重置。
6. **临时 `UnicodeString` 转 `PWideChar` 悬垂**（L-2、M2）：依赖临时对象生命周期。建议凡需持有 `PWideChar` 者一律先用存活的局部 `UnicodeString` 变量承接。
