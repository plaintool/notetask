@echo off
setlocal

:: Resolve script directory
SET "SOURCE_DIR=%~dp0"

:: Parse arguments
:: - Like main script: %1 = platform (x64/x86), %2 = version
:: - Backward compatible: if %1 is not a known platform, treat it as version (default x64)
IF /I "%~1"=="x64" (
    SET "PLATFORM=x64"
    SET "VERSION=%~2"
) ELSE IF /I "%~1"=="x86" (
    SET "PLATFORM=x86"
    SET "VERSION=%~2"
) ELSE (
    SET "PLATFORM=x64"
    SET "VERSION=%~1"
)

:: Validate platform
IF NOT "%PLATFORM%"=="x64" IF NOT "%PLATFORM%"=="x86" (
    echo Error: Invalid platform parameter. Use x64 or x86.
    echo Example: %0 x64
    echo Example: %0 x86
    exit /b 1
)

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
echo              Build Portable %PLATFORM% %VERSION%
echo ############################################################
echo.

:: Select input exe, OpenSSL DLLs and output archive name depending on platform
IF "%PLATFORM%"=="x64" (
    SET "EXE=%~dp0..\notetask.exe"
) ELSE (
    SET "EXE=%~dp0..\notetask32.exe"
)
SET "DESTZIP=%~dp0notetask-%VERSION%-%PLATFORM%-portable.zip"

powershell -NoProfile -Command ^
    "$tmp='%~dp0temp_dist';" ^
    "$exe='%EXE%';" ^
    "$portable='%~dp0portable';" ^
    "$license='%~dp0LICENSE.rtf';" ^
    "$destZip='%DESTZIP%';" ^
    "if ((Test-Path $exe)) {" ^
    "  if (Test-Path $tmp) { Remove-Item -Recurse -Force $tmp };" ^
    "  New-Item -ItemType Directory -Path \"$tmp\" -Force | Out-Null;" ^
    "  Copy-Item $exe, $portable, $license -Destination $tmp;" ^
    "  Start-Sleep -Seconds 2;" ^
    "  Compress-Archive -Force -Path \"$tmp\*\" -DestinationPath $destZip;" ^
    "  Remove-Item -Recurse -Force $tmp;" ^
    "} else { Write-Error 'Portable inputs missing'; exit 1 }"

endlocal