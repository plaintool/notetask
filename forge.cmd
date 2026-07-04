@echo off
REM Main script to run all builds

echo.
echo ######################################################################
echo #                                                                    #
echo #                         FULL BUILD STARTED                         #
echo #                                                                    #
echo ######################################################################
echo.

call build.cmd
echo Wait 2 seconds to ensure file is free
timeout /t 2 /nobreak >nul
call build32.cmd
echo Wait 2 seconds to ensure file is free
timeout /t 2 /nobreak >nul

pushd installer
call build.cmd
echo Wait 2 seconds to ensure file is free
timeout /t 2 /nobreak >nul
SET "BUILD_PORTABLE=1"
call build32.cmd
echo Wait 2 seconds to ensure file is free
timeout /t 2 /nobreak >nul
call buildinno.cmd

echo.
echo ######################################################################
echo #                                                                    #
echo #                         FULL BUILD FINISHED                        #
echo #                                                                    #
echo ######################################################################
echo.

pause