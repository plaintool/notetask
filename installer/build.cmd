@echo off
setlocal

:: Define paths
SET "SOURCE_DIR=E:\notetask\installer"
SET "VERSION=1.0.0"

:: --- Build peruser ---
echo Compiling msisetup_peruser.wxs with candle...
candle -nologo "%SOURCE_DIR%\msisetup_peruser.wxs" -out "%SOURCE_DIR%\peruser.wixobj" -ext WixUIExtension >nul
echo Linking peruser.wixobj into notetask-%VERSION%.msi with light...
light -nologo "%SOURCE_DIR%\peruser.wixobj" -out "%SOURCE_DIR%\notetask-%VERSION%.msi" -ext WixUIExtension >nul
echo File created: notetask-%VERSION%.msi
echo.

:: --- Build permachine ---
echo Compiling msisetup_permachine.wxs with candle...
candle -nologo "%SOURCE_DIR%\msisetup_permachine.wxs" -out "%SOURCE_DIR%\permachine.wixobj" -ext WixUIExtension >nul
echo Linking permachine.wixobj into notetask-%VERSION%-allusers.msi with light...
light -nologo "%SOURCE_DIR%\permachine.wixobj" -out "%SOURCE_DIR%\notetask-%VERSION%-allusers.msi" -ext WixUIExtension >nul
echo File created: notetask-%VERSION%-allusers.msi
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
%SIGNTOOL% sign /f "%CERTFILE%" /p "%CERTPASS%" /fd SHA256 /tr %TIMESTAMP_URL% /td SHA256 "%SOURCE_DIR%\notetask-%VERSION%.msi"
IF %ERRORLEVEL% EQU 0 (
    echo Signing of notetask-%VERSION%.msi completed successfully
) else (
    echo Signing failed for notetask-%VERSION%.msi
)

%SIGNTOOL% sign /f "%CERTFILE%" /p "%CERTPASS%" /fd SHA256 /tr %TIMESTAMP_URL% /td SHA256 "%SOURCE_DIR%\notetask-%VERSION%-allusers.msi"
IF %ERRORLEVEL% EQU 0 (
    echo Signing of notetask-%VERSION%-allusers.msi completed successfully
) else (
    echo Signing failed for notetask-%VERSION%-allusers.msi
)

:: --- Portable ---
tar -czf notetask-1.0.0.tar.gz -C .. notetask.exe -C "%CD%\debsetup\DATA\usr\bin" notetask

echo Build and signing completed successfully!
pause