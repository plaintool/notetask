@echo off
setlocal

:: Define paths
SET "SOURCE_DIR=%~dp0"
SET "VERSION=%VERSION%"
IF "%~2" NEQ "" (
    SET "VERSION=%~2"
)
IF "%VERSION%"=="" (
    FOR /F "usebackq delims=" %%i IN ("%SOURCE_DIR%..\VERSION") DO SET "VERSION=%%i"
)

:: --- Copying languages ---
set "source=%~dp0innolanguages"
set "destination=C:\Program Files (x86)\Inno Setup 6\Languages"
xcopy "%source%\*" "%destination%\" /y /i /s

:: --- Build inno setup ---
"C:\Program Files (x86)\Inno Setup 6\ISCC.exe" "%SOURCE_DIR%\innosetup.iss"
echo File created: notetask-any-x86-x64.exe
echo.

::Wait 2 seconds to ensure file is free
timeout /t 2 /nobreak >nul

:: --- Sign installers ---
IF "%SIGNTOOL%"=="" (
    SET "SIGNTOOL=C:\Program Files (x86)\Windows Kits\10\bin\10.0.26100.0\x64\signtool.exe"
)
IF "%CERTFILE%"=="" (
    IF EXIST "%SOURCE_DIR%AlexanderT.pfx" (
        SET "CERTFILE=%SOURCE_DIR%AlexanderT.pfx"
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

if not "%CERTFILE%"=="" (
    if exist "%CERTFILE%" (
        if exist "%SIGNTOOL%" (
            echo Signing file...
            "%SIGNTOOL%" sign /f "%CERTFILE%" /p "%CERTPASS%" /fd SHA256 /tr %TIMESTAMP_URL% /td SHA256 "%SOURCE_DIR%\notetask-%VERSION%-any-x86-x64.exe"
            IF %ERRORLEVEL% EQU 0 (
                echo Signing of notetask-%VERSION%-any-x86-x64.exe completed successfully
            ) else (
                echo Signing failed for notetask-%VERSION%-any-x86-x64.exe
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

echo Build and signing notetask-%VERSION%-any-x86-x64.exe completed successfully!
