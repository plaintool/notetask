@echo off
setlocal

:: Determine LAZARUS_DIR if not provided
if not defined LAZARUS_DIR (
    for %%D in ("C:\Lazarus" "C:\lazarus") do (
        if exist "%%~D\lazbuild.exe" (
            set "LAZARUS_DIR=%%~D"
        )
    )
)

if not defined LAZARUS_DIR (
    echo ERROR: LAZARUS_DIR is not set and Lazarus was not found automatically.
    pause
    exit /b 1
)

if not defined LAZBUILD (
    set "LAZBUILD=%LAZARUS_DIR%\lazbuild.exe"
)

if not exist "%LAZBUILD%" (
    echo ERROR: lazbuild.exe not found at "%LAZBUILD%"
    pause
    exit /b 1
)

:: Path to 32-bit FPC compiler (if not already set by caller)
if not defined FPC32 (
    for /d %%F in ("%LAZARUS_DIR%\fpc\*") do (
        if exist "%%~F\bin\i386-win32\fpc.exe" (
            set "FPC32=%%~F\bin\i386-win32\fpc.exe"
        )
    )
)
if not defined FPC32 (
    echo ERROR: 32-bit FPC compiler not found. Set FPC32_PATH or ensure i386-win32 target is installed.
    pause
    exit /b 1
)

:start_deps

echo.
echo ############################################################
echo #                 Build Empty (x86)                        #
echo ############################################################
echo.


echo Dependencies OK
exit /b 0