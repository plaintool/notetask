@echo off
setlocal

:: Define paths
SET "SOURCE_DIR=E:\notetask\installer"
SET "VERSION=1.1.1"

:: Check if platform parameter is provided
IF "%1"=="" (
    SET "PLATFORM=x64"
) ELSE (
    SET "PLATFORM=%1"
)

:: Validate platform parameter
IF NOT "%PLATFORM%"=="x64" IF NOT "%PLATFORM%"=="x86" (
    echo Error: Invalid platform parameter. Use x64 or x86.
    echo Example: %0 x64
    echo Example: %0 x86
    pause
    exit /b 1
)

:: --- Build peruser ---
echo Compiling msisetup_peruser.wxs with candle for %PLATFORM%...
candle -nologo "%SOURCE_DIR%\msisetup_peruser.wxs" -out "%SOURCE_DIR%\peruser.wixobj" -ext WixUIExtension -dPlatform=%PLATFORM% >nul
echo Linking peruser.wixobj into notetask-%VERSION%-%PLATFORM%.msi with light...
light -nologo "%SOURCE_DIR%\peruser.wixobj" -out "%SOURCE_DIR%\notetask-%VERSION%-%PLATFORM%.msi" -ext WixUIExtension >nul
echo File created: notetask-%VERSION%-%PLATFORM%.msi
echo.

:: --- Build permachine ---
echo Compiling msisetup_permachine.wxs with candle for %PLATFORM%...
candle -nologo "%SOURCE_DIR%\msisetup_permachine.wxs" -out "%SOURCE_DIR%\permachine.wixobj" -ext WixUIExtension -dPlatform=%PLATFORM% >nul
echo Linking permachine.wixobj into notetask-%VERSION%-%PLATFORM%-allusers.msi with light...
light -nologo "%SOURCE_DIR%\permachine.wixobj" -out "%SOURCE_DIR%\notetask-%VERSION%-%PLATFORM%-allusers.msi" -ext WixUIExtension >nul
echo File created: notetask-%VERSION%-%PLATFORM%-allusers.msi
echo.

:: --- Clean temporary files ---
echo Deleting temporary .wixobj and .wixpdb files...
del /q "%SOURCE_DIR%\*.wixobj" >nul
del /q "%SOURCE_DIR%\*.wixpdb" >nul
echo Cleanup completed.
echo.

:: --- Sign installers ---
SET SIGNTOOL="C:\Program Files (x86)\Windows Kits\10\bin\10.0.26100.0\x64\signtool.exe"
SET CERTFILE=AlexanderT.pfx
SET CERTPASS=1234
SET TIMESTAMP_URL=http://timestamp.digicert.com

echo Signing MSI files...
%SIGNTOOL% sign /f "%CERTFILE%" /p "%CERTPASS%" /fd SHA256 /tr %TIMESTAMP_URL% /td SHA256 "%SOURCE_DIR%\notetask-%VERSION%-%PLATFORM%.msi"
IF %ERRORLEVEL% EQU 0 (
    echo Signing of notetask-%VERSION%-%PLATFORM%.msi completed successfully
) else (
    echo Signing failed for notetask-%VERSION%-%PLATFORM%.msi
)

%SIGNTOOL% sign /f "%CERTFILE%" /p "%CERTPASS%" /fd SHA256 /tr %TIMESTAMP_URL% /td SHA256 "%SOURCE_DIR%\notetask-%VERSION%-%PLATFORM%-allusers.msi"
IF %ERRORLEVEL% EQU 0 (
    echo Signing of notetask-%VERSION%-%PLATFORM%-allusers.msi completed successfully
) else (
    echo Signing failed for notetask-%VERSION%-%PLATFORM%-allusers.msi
)

:: --- Portable ---
powershell -Command ^
"Compress-Archive ^
 -Force ^
 -Path ..\notetask.exe,..\notetask32.exe, .\form_settings.json ^
 -DestinationPath notetask-%VERSION%-x86-x64-portable.zip"

echo Build and signing completed successfully!