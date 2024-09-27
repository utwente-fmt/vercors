@echo off
setlocal

set BIN=%~dp0
set ROOT=%BIN%..

pushd %ROOT%
call mill vercors.allTests.runScript
popd

"%ROOT%\out\vercors\allTests\runScript.dest\testSuite" %*
