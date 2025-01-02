CPPFLAGS=/nologo /EHsc /O2 /GL /GS /Oi /MD /D "_UNICODE" /D "UNICODE" /Zc:inline
RFLAGS=/nologo /n /r

{src\}.cpp.obj:
	$(CPP) $(CPPFLAGS) /c $<
{src\}.rc.res:
	$(RC) $(RFLAGS) /fo$(@F) $<

Traymond.exe: options.obj rules.obj winevent.obj icons.obj traymond.obj Traymond.res
	$(CPP) $(CPPFLAGS) /Fe$(@F) $** user32.lib shell32.lib comctl32.lib advapi32.lib gdi32.lib /link /MACHINE:X86 /MANIFESTDEPENDENCY:"type='win32' name='Microsoft.Windows.Common-Controls' version='6.0.0.0' processorArchitecture='x86' publicKeyToken='6595b64144ccf1df' language='*'"

clean:
	del *.obj *.res Traymond.exe.manifest Traymond.exe
