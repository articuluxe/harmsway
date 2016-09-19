@echo off
rem -*- Mode: dos -*-
rem tar-world.bat --- tar up the world (xr style)
rem Copyright (C) 2015, 2016  Dan Harms (dan.harms)
rem Author: Dan Harms <dan.harms@xrtrading.com>
rem Created: Thursday, May 21, 2015
rem Version: 1.0
rem Modified Time-stamp: <2016-09-17 10:13:38 dharms>
rem Modified by: Dan Harms
rem Keywords: tar whole world

setlocal
setlocal enabledelayedexpansion

set verbose=
if not %1. == . (
    set verbose=v
)

if "%MSYS%". == . (
    echo "MSYS directory undefined, aborting."
    exit /b
)

set msys="%MSYS%\bin"
set tar="%msys%\tar"
set uname="%msys%\uname"
set hostname="c:\MinGW\msys\1.0\bin\hostname"
set site=xr


set curr_dir=%cd%
set script_dir=%~dp0
set dest=xr-world.tar

for /f "delims=" %%i in ('%uname%') do set os=%%i
echo os is %os%
for /f "delims=" %%i in ('%hostname%') do set host=%%i
echo host is %host%

echo Generating %curr_dir%\%dest%...

if exist %dest% (
    del %dest%
)
%tar% c%verbose%f %dest% config doc src .gdbinit .gnupg
%tar% u%verbose%f %dest% --exclude=*.elc .emacs.d
%tar% u%verbose%f %dest% --transform=s/scripts/bin/ scripts
%tar% u%verbose%f %dest% --transform=s$bash/$$ bash
%tar% u%verbose%f %dest% --transform=s$tcsh/$$ tcsh
%tar% u%verbose%f %dest% --transform=s$os/Linux/$$ os/Linux
rem don't need to copy from host here: xr installs will
rem load their renamed host file from site/xr
%tar% u%verbose%f %dest% --transform=s$site/%site%/$$ site/%site%

echo ...done generating %dest%

timeout 2

endlocal

rem tar-world.bat ends here
