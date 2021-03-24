@echo off
setlocal

rem %~dp0 is expanded pathname of the current script under NT, i.e. the "bin" directory
set BIN=%~dp0
set CPFILE=%BIN%.classpath

set JAVA_FILES=

for %%f in ( %* ) do call :for_body %%f

rem read .classpath file into CLASSPATH variable. This is not possible directly, so we iterate 
rem     over the file content (which is just a single line, so just one loop iteration)
for /F "delims=" %%c in (%CPFILE%) do set CLASSPATH=%%c

echo "compiling %JAVA_FILES%"
"%JAVA_HOME%\bin\javac" -cp "%CLASSPATH%" %JAVA_FILES%

exit /b
 
:for_body
    set ff=%1
    set gg=%ff:~0,-4%
    if "%ff%" == "%gg%.pvl" (
      echo "compiling %ff%"
      call "%BIN%\run-class.cmd" vct.main.Main --passes=pvl-compile,codegen=. "%ff%"
      set JAVA_FILES=%JAVA_FILES% "%gg%.java"
    ) else (
      set JAVA_FILES=%JAVA_FILES% "%ff%"
    )
exit /b
