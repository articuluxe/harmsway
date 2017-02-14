rem -*- Mode: bat -*-
rem start-pageant.bat --- start pageant on start up
rem Copyright (C) 2015-2017   (dan.harms)
rem Author:  <dan.harms@xrtrading.com>
rem Created: Friday, April 10, 2015
rem Version: 1.0
rem Modified Time-stamp: <2017-02-14 11:19:55 dan.harms>
rem Keywords: pageant ssh

@echo off
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
