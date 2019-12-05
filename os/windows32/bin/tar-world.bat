@echo off
rem -*- Mode: bat -*-
rem tar-world.bat --- tar up the world (windows style)
rem Copyright (C) 2015-2019  Dan Harms (dan.harms)
rem Author: Dan Harms <dan.harms@xrtrading.com>
rem Created: Thursday, May 21, 2015
rem Version: 1.0
rem Modified Time-stamp: <2019-12-06 11:11:57 Dan.Harms>
rem Modified by: Dan.Harms
rem Keywords: tar whole world

setlocal
setlocal enabledelayedexpansion

set verbose=
if not %1. == . (
    set verbose=v
)

if "%MSYS_DIR%". == . (
    echo "MSYS_DIR undefined, aborting."
    exit /b
)

if "%TAR_EXE%". == . (
   echo "TAR_EXE undefined, aborting."
   exit /b
   )

set "id=%MSYS_DIR%\id"
set "uname=%MSYS_DIR%\uname"
set "hostname=%MSYS_DIR%\hostname"
set site=xr

set curr_dir=%cd%
set script_dir=%~dp0
set dest=world.tar

for /f "delims=" %%i in ('"%id%" -nu') do set user=%%i
echo user is %user%
for /f "delims=" %%i in ('"%uname%"') do set os=%%i
echo os is %os%
for /f "delims=" %%i in ('"%hostname%"') do set host=%%i
echo host is %host%
echo site is %site%

echo Generating %curr_dir%\%dest%...

if exist %dest% (
    del %dest%
)

"%TAR_EXE%" c%verbose%f %dest% config doc src .gnupg .fonts .config .terminfo .proviso.d
"%TAR_EXE%" u%verbose%f %dest% --exclude=*.elc .emacs.d
"%TAR_EXE%" u%verbose%f %dest% --transform=s$ext$.emacs.d/ext$ ext
"%TAR_EXE%" u%verbose%f %dest% --transform=s/scripts/bin/ scripts
"%TAR_EXE%" u%verbose%f %dest% --transform=s$dotfiles/$$ dotfiles
"%TAR_EXE%" u%verbose%f %dest% --transform=s$bash/$$ bash
"%TAR_EXE%" u%verbose%f %dest% --transform=s$tcsh/$$ tcsh
"%TAR_EXE%" u%verbose%f %dest% --transform=s$user/%user%/$$ user/%user%
"%TAR_EXE%" u%verbose%f %dest% --transform=s$os/%os%/$$ os/%os%
rem The host file is under the site directory
"%TAR_EXE%" u%verbose%f %dest% --transform=s$site/%site%/host/%host%/$$ site/%site%/host/%host%
"%TAR_EXE%" u%verbose%f %dest% --transform=s$site/%site%/$$ --exclude=.git --exclude=host site/%site%
"%TAR_EXE%" u%verbose%f %dest% --transform=s$site/%site%/$$ site/%site%/.emacs.d/settings/host/%host%
"%TAR_EXE%" u%verbose%f %dest% --transform=s$site/%site%/$$ site/%site%/.emacs.d/settings/host/hosts

echo ...done generating %dest%

timeout 2

endlocal

rem tar-world.bat ends here
