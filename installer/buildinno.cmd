@echo off
setlocal

:: Define paths
SET "SOURCE_DIR=E:\notetask\installer"
SET "VERSION=1.1.0"

:: --- Build inno setup ---
"C:\Program Files (x86)\Inno Setup 6\ISCC.exe" "%SOURCE_DIR%\innosetup.iss"
echo File created: notetask-any-x86-x64.exe
echo.

::Wait 2 seconds to ensure file is free
timeout /t 2 /nobreak >nul

:: --- Sign installers ---
SET SIGNTOOL="C:\Program Files (x86)\Windows Kits\10\bin\10.0.26100.0\x64\signtool.exe"
SET CERTFILE=AlexanderT.pfx
SET CERTPASS=1234
SET TIMESTAMP_URL=http://timestamp.digicert.com

echo Signing file...
%SIGNTOOL% sign /f "%CERTFILE%" /p "%CERTPASS%" /fd SHA256 /tr %TIMESTAMP_URL% /td SHA256 "%SOURCE_DIR%\notetask-any-x86-x64.exe"
IF %ERRORLEVEL% EQU 0 (
    echo Signing of notetask-any-x86-x64.exe completed successfully
) else (
    echo Signing failed for notetask-any-x86-x64.exe
)

echo Build and signing notetask-any-x86-x64.exe completed successfully!