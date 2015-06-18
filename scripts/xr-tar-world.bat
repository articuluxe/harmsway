@echo off
rem -*- Mode: dos -*-
rem xr-tar-world.bat --- tar up the world (xr style)
rem Copyright (C) 2015  Dan Harms (dan.harms)
rem Author: Dan Harms <dan.harms@xrtrading.com>
rem Created: Thursday, May 21, 2015
rem Version: 1.0
rem Modified Time-stamp: <2015-06-18 11:05:51 dan.harms>
rem Keywords: tar whole world

setlocal
setlocal enabledelayedexpansion

set tar=c:\msys\1.0\bin\tar

set verbose=
if not %1. == . (
    set verbose=v
)

set curr_dir=%cd%
set script_dir=%~dp0
set dest=xr-world.tar

echo Generating %curr_dir%\%dest%...

if exist %dest% (
    del %dest%
)
%tar% c%verbose%f %dest% config doc .gitignore .gdbinit
%tar% u%verbose%f %dest% --transform=s/scripts/bin/ scripts
%tar% u%verbose%f %dest% --transform=s$bash/$$ bash
%tar% u%verbose%f %dest% --transform=s$tcsh/$$ tcsh
%tar% u%verbose%f %dest% --transform=s$xr/$$ xr
%tar% u%verbose%f %dest% --exclude=*.elc .emacs.d

echo ...done generating %dest%

timeout 2

endlocal

rem xr-tar-world.bat ends here
