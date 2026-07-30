@echo off
setlocal
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
::Build Lazarus project "%APP_NAME%" using lazbuild
SET "PROJECT_PATH=%APP_NAME%.lpi"
SET "BUILD_MODE=Release"

SET "LAZARUS_DIR=%LAZARUS_DIR%"
for %%D in ("%LAZARUS_DIR%" "C:\Lazarus" "C:\lazarus") do (
    if exist "%%~D\lazbuild.exe" (
        SET "LAZARUS_DIR=%%~D"
    )
)

if not exist "%LAZARUS_DIR%\lazbuild.exe" (
    echo Lazarus not found. Set LAZARUS_DIR or install Lazarus.
    if not defined CI pause
    exit /b 1
)

SET "LAZBUILD=%LAZARUS_DIR%\lazbuild.exe"

:: Prepare common lazbuild options (used for packages and project)
set "LAZBUILD_OPTS=--build-mode=%BUILD_MODE%"
IF "%ARCH%"=="32" SET "FPC32=%FPC32_PATH%"
IF "%ARCH%"=="32" if not exist "%FPC32%" (
    for /d %%F in ("%LAZARUS_DIR%\fpc\*") do (
        if exist "%%~F\bin\i386-win32\fpc.exe" (
            SET "FPC32=%%~F\bin\i386-win32\fpc.exe"
        )
    )
)
IF "%ARCH%"=="32" if not exist "%FPC32%" (
    echo 32-bit FPC compiler not found. Set FPC32_PATH.
    if not defined CI pause
    exit /b 1
)
IF "%ARCH%"=="32" set "LAZBUILD_OPTS=%LAZBUILD_OPTS% --cpu=i386 --ws=win32 --compiler=%FPC32%"

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
        ::Restore 64-bit exe back
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
    ::Restore 64-bit exe back to original name
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

::Sign the executable
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