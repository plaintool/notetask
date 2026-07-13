@echo off
setlocal

:: ============================================================
:: Universal build script for a single Lazarus dependency
:: Usage: build_dependency.cmd <Name> <SubtreePath> <RepoURL> <Branch> <LpkFile> [RevertFile] [DoPull]
::   DoPull  - set to "true" (case insensitive) to perform git subtree pull; 
::             any other value, empty or "false" skips the pull.
:: Expects LAZBUILD to point to lazbuild.exe
:: Optionally uses LAZBUILD_OPTS for additional flags (e.g. 32-bit target)
:: ============================================================

set "DEP_NAME=%~1"
set "DEP_PATH=%~2"
set "DEP_REPO=%~3"
set "DEP_BRANCH=%~4"
set "DEP_LPK=%~5"
set "DEP_REVERT=%~6"
set "DO_PULL=%~7"
set "DO_BUILD=%~8"

if "%DEP_NAME%"=="" (
    echo ERROR: Missing dependency name
    exit /b 1
)
if "%DEP_BRANCH%"=="" (
    echo ERROR: Missing dependency branch
    exit /b 1
)
if "%DEP_PATH%"=="" (
    echo ERROR: Missing subtree path
    exit /b 1
)
if "%DEP_REPO%"=="" (
    echo ERROR: Missing repo URL
    exit /b 1
)

echo.
echo ############################################################
echo                 Build %DEP_NAME% (%ARCH_LABEL%)           
echo ############################################################
echo.

:: ----- Decide whether to update submodule -----
if /i not "%DO_PULL%"=="true" (
    echo Getting current %DEP_NAME% submodule version
    git submodule update --init -- %DEP_PATH%
    if errorlevel 1 (
        echo WARNING: Failed to get current %DEP_NAME% submodule version
    )
    goto process_lpk
)

echo Checking %DEP_NAME% submodule state

:: Check for local changes in the submodule
git -C %DEP_PATH% diff --quiet
if errorlevel 1 (
    echo WARNING: %DEP_NAME% has unstaged changes, skipping submodule update
    goto process_lpk
)

:: Check for staged changes in the submodule
git -C %DEP_PATH% diff --cached --quiet
if errorlevel 1 (
    echo WARNING: %DEP_NAME% has staged changes, skipping submodule update
    goto process_lpk
)

:: No local changes – safe to update to the latest branch commit
echo Updating %DEP_NAME% submodule
git submodule update --init --remote -- %DEP_PATH%
if errorlevel 1 (
    echo WARNING: %DEP_NAME% submodule update failed, continuing with existing version
) else (
    echo %DEP_NAME% submodule updated successfully
)

goto process_lpk

:skip_update
echo Skipping %DEP_NAME% subtree update due to local changes or DO_PULL policy.

:process_lpk
:: ----- Decide whether to attempt build lpk -----
if /i not "%DO_BUILD%"=="true" (
    echo Skipping %DEP_NAME% build because DO_BUILD is not "true".
    goto skip_build
)

echo Processing Lazarus package
if exist "%DEP_LPK%" (
    echo Building %DEP_LPK%
    "%LAZBUILD%" "%DEP_LPK%" %LAZBUILD_OPTS% -q -q
    if errorlevel 1 (
        echo ERROR: %DEP_NAME% LPK build failed
        pause
        exit /b %errorlevel%
    )
    echo %DEP_NAME% LPK processed successfully

    :: Revert auto-generated changes if a revert file is specified
    if not "%DEP_REVERT%"=="" (
        if exist "%DEP_PATH%\%DEP_REVERT%" (
            git checkout -- "%DEP_PATH%\%DEP_REVERT%"
            if not errorlevel 1 (
                echo Reverted auto-changes in %DEP_REVERT%
            )
        )
    )
) else (
    echo WARNING: %DEP_LPK% not found, skipping
)

goto fin

:skip_build
echo Skipping %DEP_NAME% build lpk due to local changes or DO_BUILD policy.

:fin
exit /b 0