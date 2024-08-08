@echo off
setlocal

set BIN=%~dp0
set ROOT=%BIN%..

pushd %ROOT%
call mill vercors.main.runScript
popd

"%ROOT%\out\vercors\main\runScript.dest\patcher" %*
