rem -*- Mode: dos -*-
rem tar-world.bat --- tar up the world
rem Copyright (C) 2015  Dan Harms (dan.harms)
rem Author: Dan Harms <dan.harms@xrtrading.com>
rem Created: Thursday, May 21, 2015
rem Version: 1.0
rem Modified Time-stamp: <2015-05-21 13:23:15 dan.harms>
rem Keywords: tar whole world

@echo off
setlocal
setlocal enabledelayedexpansion

set tar=c:\msys\1.0\bin\tar
set curr_dir=%cd%
set script_dir=%~dp0
set dest=config.tar

echo Generating %curr_dir%\%dest%...

%tar% cvf %dest% config doc .gitignore
%tar% uvf %dest% --transform=s/scripts/bin/ scripts
%tar% uvf %dest% --transform=s$bash/$$ bash
%tar% uvf %dest% --transform=s$tcsh/$$ tcsh
%tar% uvf %dest% --transform=s$xr/$$ xr
%tar% uvf %dest% --exclude=*.elc .emacs.d

echo ...done generating %dest%

endlocal

rem tar-world.bat ends here
