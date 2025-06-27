@echo off
REM Build Lazarus project "notetask" using lazbuild

SET PROJECT_PATH=notetask.lpi
SET BUILD_MODE=Release

echo Building project: %PROJECT_PATH%
"C:\Lazarus\lazbuild.exe" %PROJECT_PATH% --build-mode=%BUILD_MODE%

IF %ERRORLEVEL% NEQ 0 (
    echo Build failed!
    pause
    exit /b %ERRORLEVEL%
)

echo Build completed successfully.
pause