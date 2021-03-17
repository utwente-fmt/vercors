@echo off
setlocal

rem %~dp0 is expanded pathname of the current script under NT, i.e. the "bin" directory
set BIN=%~dp0

set BOOGIE_EXE=C:\test with space\Windows\Boogie.exe
set Z3="%BIN%\..\src\main\universal\res\deps\z3\4.8.6\Windows NT\intel\bin\z3.exe"

call "%BIN%\run-class.cmd" viper.carbon.Carbon --z3Exe %Z3% %*
