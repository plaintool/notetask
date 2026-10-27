@echo off
setlocal
cd /d "%~dp0"
set "APP_NAME=notetask"

:: Determine build architecture: 64-bit by default, 32-bit if first argument is "32"
SET "ARCH=64"
SET "SKIP_DEPS=0"
SET "SKIP_BUILD=0"
SET "SKIP_SIGN=0"
SET "SKIP_BINARIES=0"
:parse_args
if "%~1"=="" goto :done_parsing
if /I "%~1"=="32" SET "ARCH=32"
if /I "%~1"=="--no-deps" SET "SKIP_DEPS=1"
if /I "%~1"=="--no-build" SET "SKIP_BUILD=1"
if /I "%~1"=="--no-sign" SET "SKIP_SIGN=1"
if /I "%~1"=="--no-binary" SET "SKIP_BINARIES=1"
shift
goto :parse_args
:done_parsing

:: Label for console output
IF "%ARCH%"=="32" (SET "ARCH_LABEL=x86") ELSE (SET "ARCH_LABEL=x64")

:: Detect if running in CI (non-interactive) environment
:: Skip kill in CI environments
if defined CI goto :start.build

TASKLIST | FINDSTR /I "%APP_NAME%.exe" >NUL
IF ERRORLEVEL 1 GOTO :start.build
:: Kill App if running (local only)
ECHO Closing process '%APP_NAME%.exe'
taskkill /F /IM %APP_NAME%.exe >NUL

:start.build
:: Build Lazarus project "%APP_NAME%" using lazbuild
SET "PROJECT_PATH=%APP_NAME%.lpi"
SET "BUILD_MODE=Release"

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

echo Lazarus not found. Set LAZARUS_DIR or install Lazarus.
if not defined CI pause
exit /b 1

:lazarus_found
:: Strip trailing backslash: lazbuild derives lazarusdir from argv[0] and rejects "C:\lazarus\"
if "%LAZARUS_DIR:~-1%"=="\" set "LAZARUS_DIR=%LAZARUS_DIR:~0,-1%"
SET "LAZBUILD=%LAZARUS_DIR%\lazbuild.exe"

:: Prepare common lazbuild options (used for packages and project)
set "LAZBUILD_OPTS=--build-mode=%BUILD_MODE%"

:: 32-bit builds: locate FPC32 and add cross-compile flags.
:: If lazbuild itself already lives in a "...32" folder, no --compiler is needed.
IF "%ARCH%"=="32" IF /I "%LAZARUS_DIR:~-3%"=="32\" goto :skip_fpc32
IF "%ARCH%"=="32" IF /I "%LAZARUS_DIR:~-2%"=="32" goto :skip_fpc32

:: 1) Explicit FPC32_PATH
IF "%ARCH%"=="32" SET "FPC32=%FPC32_PATH%"
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
    echo 32-bit FPC compiler not found. Set FPC32_PATH.
    if not defined CI pause
    exit /b 1
)
IF "%ARCH%"=="32" SET "LAZBUILD_OPTS=%LAZBUILD_OPTS% --cpu=i386 --ws=win32 --compiler="%FPC32%""

:skip_fpc32

:: Updating and building dependencies
if %SKIP_DEPS%==1 goto :skip_deps
IF "%ARCH%"=="32" (
    call "%~dp0dependencies.cmd" 32 nopull
) ELSE (
    call "%~dp0dependencies.cmd" 64 nopull
)
if %ERRORLEVEL% neq 0 (
    echo Dependency build failed!
    if not defined CI pause
    exit /b %ERRORLEVEL%
)
:skip_deps

if %SKIP_BUILD%==1 goto :skip_build
echo.
echo ############################################################
echo                      Build %ARCH_LABEL%                   
echo ############################################################
echo.

:: 32-bit build needs to protect the existing 64-bit executable
IF "%ARCH%"=="32" (
    if exist "%APP_NAME%.exe" (
        echo Renaming existing 64-bit executable...
        ren "%APP_NAME%.exe" "%APP_NAME%64.exe"
    )
)

echo Building project: %PROJECT_PATH%
"%LAZBUILD%" %PROJECT_PATH% %LAZBUILD_OPTS% -q

IF %ERRORLEVEL% NEQ 0 (
    IF "%ARCH%"=="32" (
        echo 32-bit build failed!
        :: Restore 64-bit exe back
        if exist "%APP_NAME%64.exe" ren "%APP_NAME%64.exe" "%APP_NAME%.exe"
    ) ELSE (
        echo Build failed!
    )
    if not defined CI pause
    exit /b %ERRORLEVEL%
)

:: 32-bit post-build renaming
IF "%ARCH%"=="32" (
    if exist "%APP_NAME%.exe" (
        echo Renaming 32-bit executable...
        if exist "%APP_NAME%32.exe" del /F /Q "%APP_NAME%32.exe"
        ren "%APP_NAME%.exe" "%APP_NAME%32.exe"
    )
    :: Restore 64-bit exe back to original name
    if exist "%APP_NAME%64.exe" (
        echo Restoring 64-bit executable name...
        if exist "%APP_NAME%.exe" del /F /Q "%APP_NAME%.exe"
        ren "%APP_NAME%64.exe" "%APP_NAME%.exe"
    )
)

echo Build completed successfully
:skip_build

if %SKIP_SIGN%==1 goto :skip_sign
echo.
echo ############################################################
echo               Signing %ARCH_LABEL% executable     
echo ############################################################
echo.

echo Wait 2 seconds to ensure file is free
ping 127.0.0.1 -n 3 >nul

:: Set architecture-specific EXE name
IF "%ARCH%"=="32" (
    SET "EXE_NAME=%APP_NAME%32.exe"
) ELSE (
    SET "EXE_NAME=%APP_NAME%.exe"
)

:: Certificate settings (same as before)
IF "%SIGNTOOL%"=="" (
    SET "SIGNTOOL=C:\Program Files (x86)\Windows Kits\10\bin\10.0.26100.0\x64\signtool.exe"
)
IF "%CERTFILE%"=="" (
    IF EXIST "%~dp0installer\AlexanderT.pfx" (
        SET "CERTFILE=%~dp0installer\AlexanderT.pfx"
    ) ELSE (
        IF NOT "%CERT_PFX%"=="" (
            SET "CERTFILE=%TEMP%\%APP_NAME%-cert.pfx"
            powershell -NoProfile -Command "[IO.File]::WriteAllBytes('%TEMP%\\%APP_NAME%-cert.pfx',[Convert]::FromBase64String($env:CERT_PFX))"
        ) ELSE (
            SET "CERTFILE="
        )
    )
)
SET "CERTPASS=1234"
::SET "TIMESTAMP_URL=http://timestamp.digicert.com"
SET "TIMESTAMP_URL=http://timestamp.sectigo.com"
::SET "TIMESTAMP_URL=http://ts.ssl.com"

:: Sign the executable
if exist "%EXE_NAME%" (
    if not "%CERTFILE%"=="" (
        if exist "%CERTFILE%" (
            if exist "%SIGNTOOL%" (
                echo Signing %EXE_NAME%...
                "%SIGNTOOL%" sign /f "%CERTFILE%" /p "%CERTPASS%" /fd SHA256 /tr %TIMESTAMP_URL% /td SHA256 "%EXE_NAME%" < nul
                IF %ERRORLEVEL% EQU 0 (
                    echo Executable signing completed successfully
                ) else (
                    echo Executable signing failed
                    if not defined CI pause
                    exit /b %ERRORLEVEL%
                )
            ) else (
                echo Skipping signing: signtool not found.
            )
        ) else (
            echo Skipping signing: cert file not found.
        )
    ) else (
        echo Skipping signing: CERTFILE not set.
    )
) else (
    echo Skipping signing: executable %EXE_NAME% missing.
    if not defined CI pause
    exit /b 1
)
:skip_sign

:: Copy and sign binaries using external script
if %SKIP_BINARIES%==1 goto :skip_binaries
echo.
echo Processing binaries...
call "%~dp0depsbinary.cmd" %ARCH% %APP_NAME%
if %ERRORLEVEL% neq 0 (
    echo Binaries processing failed!
    if not defined CI pause
    exit /b %ERRORLEVEL%
)
:skip_binaries

echo All done.