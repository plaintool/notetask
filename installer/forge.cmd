@echo off
REM Main script to run all builds

echo Starting all builds...

call build.cmd x64
echo Wait 2 seconds to ensure file is free
timeout /t 2 /nobreak >nul
SET "BUILD_PORTABLE=1"
call build.cmd x86
echo Wait 2 seconds to ensure file is free
timeout /t 2 /nobreak >nul
call buildinno.cmd

echo All builds finished.
pause