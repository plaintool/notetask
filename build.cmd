@echo off
setlocal

::Build Lazarus project "notetask" using lazbuild
SET PROJECT_PATH=notetask.lpi
SET BUILD_MODE=Release

echo Building project: %PROJECT_PATH%
"C:\Lazarus\lazbuild.exe" %PROJECT_PATH% --build-mode=%BUILD_MODE%

IF %ERRORLEVEL% NEQ 0 (
    echo Build failed!
    pause
    exit /b %ERRORLEVEL%
)

echo Build completed successfully

::Certificate settings
SET SIGNTOOL="C:\Program Files (x86)\Windows Kits\10\bin\10.0.26100.0\x64\signtool.exe"
SET CERTFILE=%~dp0installer\AlexanderT.pfx
SET CERTPASS=1234
SET TIMESTAMP_URL=http://timestamp.digicert.com

::Sign the executable in the same folder
if exist "notetask.exe" (
    echo Signing executable...
    %SIGNTOOL% sign /f "%CERTFILE%" /p "%CERTPASS%" /fd SHA256 /tr %TIMESTAMP_URL% /td SHA256 "notetask.exe"
    IF %ERRORLEVEL% EQU 0 (
        echo Signing completed successfully
    ) else (
        echo Signing failed
    )
) else (
    echo Executable not found: notetask.exe
)