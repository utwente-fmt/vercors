@echo off
setlocal

set BIN=%~dp0
set ROOT=%BIN%..

pushd %ROOT%
call mill vercors.runScript
popd


set BOOGIE_EXE="%ROOT%\res\universal\deps\win\boogie\Boogie.exe"
set Z3="%ROOT%\res\universal\deps\win\z3\bin\z3.exe"

call "%ROOT%\out\vercors\runScript.dest\carbon" --z3Exe %Z3% %*