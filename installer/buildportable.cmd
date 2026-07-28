@echo off
setlocal

:: Resolve script directory
SET "SOURCE_DIR=%~dp0"
SET "VERSION=%~1"

:: If no version passed, try to read from ..\VERSION
IF "%VERSION%"=="" (
    IF EXIST "%SOURCE_DIR%..\VERSION" (
        FOR /F "usebackq delims=" %%i IN ("%SOURCE_DIR%..\VERSION") DO SET "VERSION=%%i"
    )
)

:: Still no version? Error out
IF "%VERSION%"=="" (
    echo ERROR: Version not specified. Pass as argument or place a VERSION file in the parent directory.
    exit /b 1
)

echo.
echo ############################################################
echo                   Build Portable %VERSION%                    
echo ############################################################
echo.

    powershell -NoProfile -Command ^
    "$tmp='%~dp0temp_dist';" ^
    "$exe64='%~dp0..\\notetask.exe';" ^
    "$exe32='%~dp0..\\notetask32.exe';" ^
    "$settings='%~dp0form_settings.json';" ^
    "$license='%~dp0LICENSE.rtf';" ^
    "$destZip='%~dp0notetask-%VERSION%-x86-x64-portable.zip';" ^
    "if ((Test-Path $exe64) -and (Test-Path $exe32)) {" ^
    "  if (Test-Path $tmp) { Remove-Item -Recurse -Force $tmp };" ^
    "  New-Item -ItemType Directory -Path \"$tmp\" -Force | Out-Null;" ^
    "  Copy-Item $exe64, $exe32, $settings, $license -Destination $tmp;" ^
    "  Start-Sleep -Seconds 2;" ^
    "  Compress-Archive -Force -Path \"$tmp\\*\" -DestinationPath $destZip;" ^
    "  Remove-Item -Recurse -Force $tmp;" ^
    "} else { Write-Error 'Portable inputs missing'; exit 1 }"

endlocal