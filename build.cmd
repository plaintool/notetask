@echo off
setlocal

::Build Lazarus project "notetask" using lazbuild
SET "PROJECT_PATH=notetask.lpi"
SET "BUILD_MODE=Release"

SET "LAZARUS_DIR=%LAZARUS_DIR%"
for %%D in ("%LAZARUS_DIR%" "C:\Lazarus" "C:\lazarus") do (
    if exist "%%~D\lazbuild.exe" (
        SET "LAZARUS_DIR=%%~D"
    )
)

if not exist "%LAZARUS_DIR%\lazbuild.exe" (
    echo Lazarus not found. Set LAZARUS_DIR or install Lazarus.
    exit /b 1
)

SET "LAZBUILD=%LAZARUS_DIR%\lazbuild.exe"

echo Building project: %PROJECT_PATH%
"%LAZBUILD%" %PROJECT_PATH% --build-mode=%BUILD_MODE%

IF %ERRORLEVEL% NEQ 0 (
    echo Build failed!
    exit /b %ERRORLEVEL%
)

echo Build completed successfully

echo Wait 2 seconds to ensure file is free
ping 127.0.0.1 -n 3 >nul

::Certificate settings (optional)
IF "%SIGNTOOL%"=="" (
    SET "SIGNTOOL=C:\Program Files (x86)\Windows Kits\10\bin\10.0.26100.0\x64\signtool.exe"
)
IF "%CERTFILE%"=="" (
    IF EXIST "%~dp0installer\AlexanderT.pfx" (
        SET "CERTFILE=%~dp0installer\AlexanderT.pfx"
    ) ELSE (
        IF NOT "%CERT_PFX%"=="" (
            SET "CERTFILE=%TEMP%\notetask-cert.pfx"
            powershell -NoProfile -Command "[IO.File]::WriteAllBytes('%TEMP%\\notetask-cert.pfx',[Convert]::FromBase64String($env:CERT_PFX))"
        ) ELSE (
            SET "CERTFILE="
        )
    )
)
SET "CERTPASS=1234"
SET "TIMESTAMP_URL=http://timestamp.digicert.com"

::Sign the executable in the same folder
if exist "notetask.exe" (
    if not "%CERTFILE%"=="" (
        if exist "%CERTFILE%" (
            if exist "%SIGNTOOL%" (
                echo Signing executable...
                "%SIGNTOOL%" sign /f "%CERTFILE%" /p "%CERTPASS%" /fd SHA256 /tr %TIMESTAMP_URL% /td SHA256 "notetask.exe"
                IF %ERRORLEVEL% EQU 0 (
                    echo Signing completed successfully
                ) else (
                    echo Signing failed
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
    echo Skipping signing: missing executable.
)
