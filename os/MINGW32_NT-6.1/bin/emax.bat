@echo off
rem -*- Mode: bat -*-
rem emax.bat --- start emacs client on windows
rem Copyright (C) 2018  Dan Harms (dharms)
rem Author: Dan Harms <enniomore@icloud.com>
rem Created: Thursday, August  9, 2018
rem Version: 1.0
rem Modified Time-stamp: <2018-08-09 15:38:27 dharms>
rem Modified by: Dan Harms
rem Keywords: emacs tools

setlocal
setlocal enabledelayedexpansion

if "%EMACS%". == . (
    echo "EMACS undefined, aborting."
    exit /b
)

%EMACS%\bin\emacsclientw.exe -n -c -a ""

endlocal

rem code ends here
