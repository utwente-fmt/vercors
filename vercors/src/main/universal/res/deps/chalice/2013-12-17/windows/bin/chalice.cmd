@echo off

for /f %%i in ('where Boogie.exe') do set BOOGIE_CMD=%%i

java -cp %CHALICE_HOME%\chalice_2.10-1.0.jar;%CHALICE_HOME%\scala-library.jar chalice.Chalice /boogie:%BOOGIE_CMD% %*

