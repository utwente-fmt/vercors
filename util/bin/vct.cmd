@echo off
setlocal

rem %~dp0 is expanded pathname of the current script under NT, i.e. the "bin" directory
set BIN=%~dp0

call "%BIN%\run-class.cmd" vct.main.Main %*
