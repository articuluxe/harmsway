@echo off
rem -*- Mode: bat -*-
rem start-pageant.bat --- start pageant on start up
rem Copyright (C) 2015-2019   (dan.harms)
rem Author:  <dan.harms@xrtrading.com>
rem Created: Friday, April 10, 2015
rem Version: 1.0
rem Modified Time-stamp: <2019-12-06 09:28:32 Dan.Harms>
rem Keywords: pageant ssh

setlocal
setlocal enabledelayedexpansion

if "%HOME%". == . (
    echo "HOME directory undefined, aborting."
    exit /b
)
if "%PUTTY_DIR%". == . (
    echo "PUTTY_DIR directory undefined, aborting."
    exit /b
)

set "exe=%PUTTY_DIR%\pageant.exe"
set "key_file=%HOME%\.ssh\Dan.Harms.ppk"

start %exe% %key_file%

endlocal

rem start-pageant.bat ends here
