# ![Traymond](images/logo.png) Traymond（增强版)

Traymond（增强版）是一款能将任意窗口最小化到系统托盘的 Windows 桌面工具。此项目受 [Traymond](https://github.com/fcFn/traymond) 启发，实现以下功能：

- 中文界面
- 开机自动运行
- 可自定义热键
  - 最小化窗口到系统托盘图标或右键菜单
  - 还原最后一个最小化窗口
  - 唤出最小化窗口列表
- 通过自定义规则自动最小化窗口

## 安装

1. 从 https://github.com/tabris17/traymond/releases/latest 下载可执行文件直接运行；

2. 使用 Scoop

   ```cmd
   scoop install https://github.com/tabris17/traymond/releases/latest/download/traymond.json
   ```

## 用法

程序运行后会常驻系统托盘。按下默认热键 <kbd>Win</kbd> + <kbd>Shift</kbd> + <kbd>Z</kbd> 最小化当前窗口到系统托盘。可以在“选项”中设置最小化窗口收纳至托盘图标或者右键菜单。

![popup menu](images/popup-menu.png)

鼠标双击托盘图标 ![icon](images/logo-sm.png) 或者在右键菜单中选择“选项”，打开“Traymond 选项” 对话框。

![Traymond 选项](images/options.png)

### 自定义热键

在“自定义热键”列表内选中需要设置热键的行为，在下方的热键控件中按下自定义热键组合。由于热键控件无法响应按下 <kbd>Win</kbd> 键，如果要使用 <kbd>Win</kbd> 作为修饰键，请勾选“使用 Win 键” 选择框。

![设置热键](images/options-hotkey.png)

### 自动最小化窗口

在“Traymond 选项”对话框中勾选“运行期间自动最小化指定窗口”选择框，以启用自动最小化窗口功能。点击“自定义规则”按钮设置规则。

![自定义隐藏窗口规则](images/rules.png)

具体设置方法请参考项目[维基](https://github.com/tabris17/traymond/wiki)。

## 贡献指南

### 环境配置

由于本项目依托 Windows 系统的 API 运行, 故需要在 Windows 环境中构建, 调试

- 下载 [Visual Studio Build Tools](https://visualstudio.microsoft.com/zh-hans/visual-cpp-build-tools/), 打开后在 Individual components 选项卡中：

  - MSVC C++ 的组件中任选一个心仪的版本
  - 选择一个 Windows SDK (根据 Windows 版本选择)
  - 确认, 安装

  建议不要更改下载路径, 虽然它真的很大, 如果决定更改, 请确保路径中没有中文字符, 并在后续的步骤中自行替换相应的路径

- 在系统变量中添加如下环境变量(版本号, 具体路径根据实际情况修改, 一些实用的建议见 FAQ )

  - LIB
    - C:\Program Files (x86)\Windows Kits\10\Lib\10.0.20348.0\um\x86
    - C:\Program Files (x86)\Microsoft Visual Studio\2022\BuildTools\VC\Tools\MSVC\14.36.32532\lib\x86
    - C:\Program Files (x86)\Windows Kits\10\Lib\10.0.20348.0\ucrt\x86
  - INCLUDE
    - C:\Program Files (x86)\Windows Kits\10\Include\10.0.20348.0\shared
    - C:\Program Files (x86)\Windows Kits\10\Include\10.0.20348.0\ucrt
    - C:\Program Files (x86)\Windows Kits\10\Include\10.0.20348.0\um
    - C:\Program Files (x86)\Microsoft Visual Studio\2022\BuildTools\VC\Tools\MSVC\14.36.32532\include

- 重启电脑
- 在 PowerShell 中 cd 到项目目录, 执行`nmake`
- 如果没有报错, 说明编译成功, 在项目目录下会生成一个名为 traymond.exe 的可执行文件(若有报错, 见 FAQ)

### FAQ

- 我不知道具体下载哪些东西

  本人下载了

  - Windows 11 SDK (10.0.220000)
  - MSVC v143 - VS 2022 C++ x64/x86 build tools

- 执行 nmake 时提示无法识别某个命令 / 找不到某个文件

  确保你使用的是 Windows 环境的 Shell 而不是 WSL, 确保正确设置了环境变量, 使用 Linux 环境下的 find 命令或许会很有帮助

- 我不知如何下手进行我的更改

  或许 Copilot , Linux 环境下的 grep 命令会很有帮助

- nmake 是什么

  nmake 是 Windows 下的 make 工具, 对 make 不太熟悉? 这份[深入浅出的文档](https://seisman.github.io/how-to-write-makefile/)或许会很有帮助

- 如何调试

  见 logging.h

- 有没有什么建议

  谨慎使用 AI 进行大量的代码更改, 理由见[木遥的博客 Mar, 28, 2025](https://blog.farmostwood.net/author/farmostwood)
