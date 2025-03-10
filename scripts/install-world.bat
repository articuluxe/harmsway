@echo off
rem -*- Mode: bat -*-
rem install-world.bat --- install the world (win32 style)
rem Copyright (C) 2017, 2019-2020  Dan Harms (dan.harms)
rem Author: Dan Harms <enniomore@icloud.com>
rem Created: Thursday, December 21, 2017
rem Version: 1.0
rem Modified Time-stamp: <2020-09-08 23:39:38 dharms>
rem Modified by: Dan Harms
rem Keywords: install perfect editor
setlocal
setlocal enabledelayedexpansion

if "%HOME%". == . (
    echo "HOME directory undefined, aborting."
    exit /b
)
set dir=%HOME%\src\harmsway
if not exist %dir% (
    echo "%dir% does not exist, aborting."
    exit /b
)
set file=world.tar

cd %dir%
call %HOME%\bin\tar-world.bat
if not exist %file% (
    echo "%file% does not exist in %dir%, aborting."
    exit /b
)

move /Y world.tar %HOME%
cd "%HOME%"
call %HOME%\bin\untar-world.bat

endlocal

rem install-world.bat ends here
