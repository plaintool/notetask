@echo off
setlocal

if "%~1"=="" (
    echo Error: please specify the submodule name.
    echo Usage: depadd.cmd synapse [branch]
    exit /b 1
)

set "PACKAGE=%~1"
set "BRANCH=%~2"

if "%BRANCH%"=="" set "BRANCH=main"

set "REMOTE_NAME=libs/%PACKAGE%"
set "REMOTE_URL=https://github.com/plainlib/%PACKAGE%.git"

echo Cleaning old files...

if exist "libs\%PACKAGE%" (
    rmdir /s /q "libs\%PACKAGE%"
)

if exist ".git\modules\libs\%PACKAGE%" (
    rmdir /s /q ".git\modules\libs\%PACKAGE%"
)

timeout /t 1 /nobreak >nul

echo Adding submodule "%PACKAGE%" from branch "%BRANCH%"...

git submodule add --force -b "%BRANCH%" "%REMOTE_URL%" "%REMOTE_NAME%"

if errorlevel 1 (
    echo Failed to add submodule.
    exit /b 1
)

echo Submodule "%PACKAGE%" added successfully.