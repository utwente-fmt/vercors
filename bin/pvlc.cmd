@echo off

rem %~dp0 is expanded pathname of the current script under NT
rem because of problem with %cd% was used instead of %~dp0..\..\ 
rem in configuration.java --> getparent of %~dp0..\..\ is %~dp0..\ which does not seem to work

@echo off

rem %~dp0 is expanded pathname of the current script under NT

set VCT_HOME=%~dp0..\..\
set TOOL_HOME=%VCT_HOME%..\..\

rem Create class path with run time libraries

set VCT_PATH=%CLASSPATH%;%VCT_HOME%\hre\bin
set VCT_PATH=%VCT_PATH%;%VCT_HOME%\hre\hre.jar
set VCT_PATH=%VCT_PATH%;%VCT_HOME%\core\bin
set VCT_PATH=%VCT_PATH%;%VCT_HOME%\core\vct-core.jar
set VCT_PATH=%VCT_PATH%;%VCT_HOME%\core\libs\antlr-4.5-complete.jar
set VCT_PATH=%VCT_PATH%;%VCT_HOME%\libs\commons-lang3-3.1\commons-lang3-3.1.jar
set VCT_PATH=%VCT_PATH%;%VCT_HOME%\main\bin
set VCT_PATH=%VCT_PATH%;%VCT_HOME%\main\vct-tool.jar
set VCT_PATH=%VCT_PATH%;%VCT_HOME%\libs\junit_4.11.0.jar
set VCT_PATH=%VCT_PATH%;%VCT_HOME%\libs\tempus-fugit-1.1.jar
set VCT_PATH=%VCT_PATH%;%VCT_HOME%\libs\hamcrest.core_1.3.0.jar

set JAVA_FILES=

for %%f in ( %* ) do call :for_body %%f

%JAVA_HOME%\bin\javac -cp %VCT_PATH% %JAVA_FILES%

exit /b
 
:for_body
    set ff=%1
    set gg=%ff:~0,-4%
    if %ff% == %gg%.pvl (
      java -Xss128M -XX:-UseSplitVerifier -cp %VCT_PATH% vct.main.Main --passes=pvl-compile,codegen=. %ff%
      set JAVA_FILES=%JAVA_FILES% %gg%.java
    ) else (
      set JAVA_FILES=%JAVA_FILES% %ff%
    )
exit /b
