@echo off
setlocal

:: Determine build architecture: 64-bit by default, 32-bit if first argument is "32"
SET "ARCH=64"
IF /I "%1"=="32" SET "ARCH=32"

:: Determine if subtree pull should be performed (default yes)
SET "DO_PULL=true"
IF /I "%2"=="nopull" SET "DO_PULL=false"
IF /I "%2"=="false"  SET "DO_PULL=false"

:: Determine if packages should be built (default yes)
SET "DO_BUILD=true"
IF /I "%3"=="nobuild" SET "DO_BUILD=false"
IF /I "%3"=="false"   SET "DO_BUILD=false"

:: Label for console output
IF "%ARCH%"=="32" (SET "ARCH_LABEL=x86") ELSE (SET "ARCH_LABEL=x64")

:: Find Lazarus build tool - try LAZARUS_DIR, PATH, registry, then common folders
if defined LAZARUS_DIR if exist "%LAZARUS_DIR%\lazbuild.exe" goto :lazarus_found
for /f "delims=" %%P in ('where lazbuild.exe 2^>nul') do (set "LAZARUS_DIR=%%~dpP" & goto :lazarus_found)
for /f "tokens=2*" %%A in ('reg query "HKLM\SOFTWARE\Lazarus" /v InstallDir 2^>nul ^| findstr InstallDir') do (
    if exist "%%B\lazbuild.exe" (set "LAZARUS_DIR=%%B" & goto :lazarus_found)
)

:: For 32-bit builds, prefer a dedicated 32-bit Lazarus installation if present
if "%ARCH%"=="32" (
    for %%D in (
        "D:\lazarus32" "C:\lazarus32" "E:\lazarus32"
        "D:\Lazarus32" "C:\Lazarus32" "E:\Lazarus32"
        "%ProgramFiles(x86)%\Lazarus" "%ProgramFiles%\Lazarus32"
    ) do (
        if exist "%%~D\lazbuild.exe" (set "LAZARUS_DIR=%%~D" & goto :lazarus_found)
    )
)

:: Fallback to any Lazarus installation
for %%D in (
    "C:\Lazarus" "C:\lazarus"
    "D:\Lazarus" "D:\lazarus"
    "E:\Lazarus" "E:\lazarus"
    "C:\tools\lazarus" "C:\tools\Lazarus"
    "%ProgramFiles%\Lazarus" "%ProgramFiles(x86)%\Lazarus"
) do (
    if exist "%%~D\lazbuild.exe" (set "LAZARUS_DIR=%%~D" & goto :lazarus_found)
)

echo ERROR: LAZARUS_DIR is not set and Lazarus was not found automatically.
if not defined CI pause
exit /b 1

:lazarus_found
:: Strip trailing backslash: lazbuild derives lazarusdir from argv[0] and rejects "C:\lazarus\"
if "%LAZARUS_DIR:~-1%"=="\" set "LAZARUS_DIR=%LAZARUS_DIR:~0,-1%"
if not defined LAZBUILD (
    set "LAZBUILD=%LAZARUS_DIR%\lazbuild.exe"
)

if not exist "%LAZBUILD%" (
    echo ERROR: lazbuild.exe not found at "%LAZBUILD%"
    if not defined CI pause
    exit /b 1
)

:: 32-bit builds: inherit FPC32 from caller (build script) if set, otherwise locate it.
:: If lazbuild itself already lives in a "...32" folder, no --compiler is needed.
IF "%ARCH%"=="32" IF /I "%LAZARUS_DIR:~-3%"=="32\" goto :skip_fpc32
IF "%ARCH%"=="32" IF /I "%LAZARUS_DIR:~-2%"=="32" goto :skip_fpc32

:: 1) Inherit from caller or explicit FPC32_PATH
IF "%ARCH%"=="32" IF NOT DEFINED FPC32 SET "FPC32=%FPC32_PATH%"
IF "%ARCH%"=="32" IF NOT EXIST "%FPC32%" SET "FPC32="
:: 2) Bundled / sibling FPC folders (first match wins; preferred over PATH,
::    because PATH may contain a 64-bit FPC that cannot target i386-win32)
IF "%ARCH%"=="32" (
    for /d %%F in (
        "%LAZARUS_DIR%\fpc\*"
        "%LAZARUS_DIR%\..\lazarus32\fpc\*"
        "%LAZARUS_DIR%\..\lazarus\fpc\*"
        "%LAZARUS_DIR%\..\fpc\*"
        "C:\FPC\*" "D:\FPC\*" "C:\fpc\*" "D:\fpc\*"
    ) do (
        if not defined FPC32 if exist "%%~F\bin\i386-win32\fpc.exe" set "FPC32=%%~F\bin\i386-win32\fpc.exe"
    )
)
:: 3) fpc.exe from PATH (last resort)
IF "%ARCH%"=="32" IF NOT EXIST "%FPC32%" (
    for /f "delims=" %%P in ('where fpc.exe 2^>nul') do set "FPC32=%%P"
)
IF "%ARCH%"=="32" IF NOT EXIST "%FPC32%" (
    echo ERROR: 32-bit FPC compiler not found. Set FPC32 or ensure i386-win32 target is installed.
    if not defined CI pause
    exit /b 1
)
IF "%ARCH%"=="32" set "LAZBUILD_OPTS=--cpu=i386 --ws=win32 --compiler="%FPC32%""

:skip_fpc32

cd /d "%~dp0"

:: Jump to the main part to avoid executing the subroutine
goto :Main

:: Build a single component with all parameters passed explicitly
:BuildComponent
set "comp=%~1"
set "lower=%~2"
set "branch=%~3"
set "lpkname=%~4"
set "revert=%~5"
set "pullflag=%~6"
set "buildflag=%~7"

:: If lpk file(s) are specified, pass them as a comma-separated list
:: relative to the submodule root; otherwise leave empty.
if not "%lpkname%"=="" (
    set "lpkfull=%lpkname%"
) else (
    set "lpkfull="
)

call "%~dp0dependency.cmd" ^
    "%comp%" ^
    "libs/%lower%" ^
    "https://github.com/plainlib/%lower%.git" ^
    "%branch%" ^
    "%lpkfull%" ^
    "%revert%" ^
    %pullflag% %buildflag%

if errorlevel 1 (
    if not defined CI pause
    exit /b %errorlevel%
)
exit /b 0

:: Main part
:Main

:: Build DarkMode
call :BuildComponent DarkMode darkmode main darkmode.lpk "" %DO_PULL% %DO_BUILD%

:: Build DCPCrypt
call :BuildComponent DCPCrypt dcpcrypt main dcpcrypt.lpk "" %DO_PULL% %DO_BUILD%

:: Build FileManager
call :BuildComponent FileManager filemanager main filemanager.lpk "" %DO_PULL% %DO_BUILD%

:: Build GridPrinter
call :BuildComponent GridPrinter gridprinter main gridprinter.lpk "" %DO_PULL% %DO_BUILD%

:: Build Helpers
call :BuildComponent Helpers helpers main helpers.lpk "" %DO_PULL% %DO_BUILD%

:: Build TagEdit
call :BuildComponent TagEdit tagedit main tageditpackage.lpk "" %DO_PULL% %DO_BUILD%

:: Build Toolkit
call :BuildComponent Toolkit toolkit main toolkit.lpk "" %DO_PULL% %DO_BUILD%

echo.
echo Dependencies OK
exit /b 0