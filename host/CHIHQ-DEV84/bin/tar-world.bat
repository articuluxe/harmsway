@echo off
rem -*- Mode: dos -*-
rem tar-world.bat --- tar up the world (windows style)
rem Copyright (C) 2015-2018  Dan Harms (dan.harms)
rem Author: Dan Harms <dan.harms@xrtrading.com>
rem Created: Thursday, May 21, 2015
rem Version: 1.0
rem Modified Time-stamp: <2018-07-12 22:45:44 dharms>
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
set id="%msys%\id"
set uname="%msys%\uname"
set hostname="c:\MinGW\msys\1.0\bin\hostname"
set site=xr

set curr_dir=%cd%
set script_dir=%~dp0
set dest=world.tar

for /f "delims=" %%i in ('%id% -nu') do set user=%%i
echo user is %user%
for /f "delims=" %%i in ('%uname%') do set os=%%i
echo os is %os%
for /f "delims=" %%i in ('%hostname%') do set host=%%i
echo host is %host%
echo site is %site%

echo Generating %curr_dir%\%dest%...

if exist %dest% (
    del %dest%
)
%tar% c%verbose%f %dest% config doc src .gdbinit .gnupg .fonts
%tar% u%verbose%f %dest% --exclude=*.elc .emacs.d
%tar% u%verbose%f %dest% --transform=s$ext$.emacs.d/ext$ ext
%tar% u%verbose%f %dest% --transform=s/scripts/bin/ scripts
%tar% u%verbose%f %dest% --transform=s$bash/$$ bash
%tar% u%verbose%f %dest% --transform=s$tcsh/$$ tcsh
%tar% u%verbose%f %dest% --transform=s$user/%user%/$$ user/%user%
%tar% u%verbose%f %dest% --transform=s$os/%os%/$$ os/%os%
%tar% u%verbose%f %dest% --transform=s$host/%host%/$$ host/%host%
%tar% u%verbose%f %dest% --transform=s$site/%site%/$$ --exclude=.git site/%site%

echo ...done generating %dest%

timeout 2

endlocal

rem tar-world.bat ends here
