@echo off
REM Main script to run all builds

echo Starting all builds...

call build.cmd
echo Wait 2 seconds to ensure file is free
timeout /t 2 /nobreak >nul
call build32.cmd
echo Wait 2 seconds to ensure file is free
timeout /t 2 /nobreak >nul

pushd installer
call build.cmd
echo Wait 2 seconds to ensure file is free
timeout /t 2 /nobreak >nul
call build32.cmd
echo Wait 2 seconds to ensure file is free
timeout /t 2 /nobreak >nul
call buildinno.cmd

echo All builds finished.
pause