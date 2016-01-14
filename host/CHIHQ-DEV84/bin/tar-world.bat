@echo off
rem -*- Mode: dos -*-
rem tar-world.bat --- tar up the world (xr style)
rem Copyright (C) 2015, 2016  Dan Harms (dan.harms)
rem Author: Dan Harms <dan.harms@xrtrading.com>
rem Created: Thursday, May 21, 2015
rem Version: 1.0
rem Modified Time-stamp: <2016-01-14 16:37:17 dan.harms>
rem Modified by: Dan Harms
rem Keywords: tar whole world

setlocal
setlocal enabledelayedexpansion

set tar=c:\msys\1.0\bin\tar
set uname=c:\msys\1.0\bin\uname
set hostname=c:\MinGW\msys\1.0\bin\hostname

set verbose=
if not %1. == . (
    set verbose=v
)

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
%tar% c%verbose%f %dest% config doc src .gdbinit
%tar% u%verbose%f %dest% --exclude=*.elc .emacs.d
%tar% u%verbose%f %dest% --transform=s/scripts/bin/ scripts
%tar% u%verbose%f %dest% --transform=s$bash/$$ bash
%tar% u%verbose%f %dest% --transform=s$tcsh/$$ tcsh
%tar% u%verbose%f %dest% --transform=s$os/%os%/$$ os/%os%
%tar% u%verbose%f %dest% --transform=s$host/%host%/$$ host/%host%

echo ...done generating %dest%

timeout 2

endlocal

rem tar-world.bat ends here
