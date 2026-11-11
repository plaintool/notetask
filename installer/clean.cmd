@echo off
setlocal enabledelayedexpansion

set "folder=%~dp0"

for %%F in ("%folder%\*") do (
    if not exist "%%F\NUL" (
        set "ext=%%~xF"
        if /I not "%%~nxF"=="portable" if /I not "!ext!"==".wxs" if /I not "!ext!"==".iss" if /I not "!ext!"==".rtf" if /I not "!ext!"==".cmd" if /I not "!ext!"==".pfx" if /I not "!ext!"==".xml" if /I not "!ext!"==".json" if /I not "!ext!"==".png" (
            del "%%F"
        )
    )
)