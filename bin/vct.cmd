@echo off
setlocal

set BIN=%~dp0
set ROOT=%BIN%..

pushd %ROOT%
mill vercors.runScript
popd

"%ROOT%out\vercors\runScript.dest\vercors" %*