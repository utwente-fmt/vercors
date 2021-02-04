@echo off
setlocal

rem %~dp0 is expanded pathname of the current script under NT, i.e. the "bin" directory
set BIN=%~dp0
set CPFILE=%BIN%.classpath

rem remember current directory
set cur=%cd%

rem extract classpath from SBT if not already cached
if not exist "%CPFILE%" (
    echo Extracting classpath from SBT. This might take a moment.
    cd %~dp0..
    rem get classpath from SBT
    rem |                                extract lines that do NOT start with "[" (using regex)
    rem |                                |                    save that to .classpath file
    sbt "export compile:fullClasspath" | findstr /R /V "^\[" > "%CPFILE%"
    cd %cur%
    echo Classpath extracted
)

rem read .classpath file into CLASSPATH variable. This is not possible directly, so we iterate 
rem     over the file content (which is just a single line, so just one loop iteration)
for /F "delims=" %%c in (%CPFILE%) do set CLASSPATH=%%c

rem execute java with classpath from above and given arguments
java -Xss128M -cp "%CLASSPATH%" %*
