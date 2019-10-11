@echo off

rem %~dp0 is expanded pathname of the current script under NT
rem because of problem with %cd% was used instead of %~dp0..\..\
rem in configuration.java --> getparent of %~dp0..\..\ is %~dp0..\ which does not seem to work

@echo off

rem %~dp0 is expanded pathname of the current script under NT

setlocal
set VCT_HOME=%~dp0..\..\

rem Create class path with run time libraries

set VCT_PATH=%VCT_HOME%\hre\bin
set VCT_PATH=%VCT_PATH%;%VCT_HOME%\parsers\bin
set VCT_PATH=%VCT_PATH%;%VCT_HOME%\vercors\target\scala-2.12\vercors-assembly-1.0.0.jar

java -Xss128M -cp "%VCT_PATH%" vct.main.Main %*
