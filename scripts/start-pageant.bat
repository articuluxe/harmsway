@echo off
rem -*- Mode: bat -*-
rem start-pageant.bat --- start pageant on start up
rem Copyright (C) 2015-2018   (dan.harms)
rem Author:  <dan.harms@xrtrading.com>
rem Created: Friday, April 10, 2015
rem Version: 1.0
rem Modified Time-stamp: <2018-07-26 09:05:14 dan.harms>
rem Keywords: pageant ssh

setlocal
setlocal enabledelayedexpansion

if "%HOME%". == . (
    echo "HOME directory undefined, aborting."
    exit /b
)
if "%PUTTY%". == . (
    echo "PUTTY directory undefined, aborting."
    exit /b
)

set exe="%PUTTY%\pageant.exe"
set key_dir="%HOME%\.ssh"
set key_file=Dan.Harms.ppk

start %exe% %key_dir%\%key_file%

endlocal

rem start-pageant.bat ends here
