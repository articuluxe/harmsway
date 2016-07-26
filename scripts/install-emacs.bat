rem -*- Mode: dos -*-
rem xr-install-emacs.bat --- install emacs (xr style)
rem Copyright (C) 2015, 2016  Dan Harms (dan.harms)
rem Author: Dan Harms <dan.harms@xrtrading.com>
rem Created: Thursday, May 21, 2015
rem Version: 1.0
rem Modified Time-stamp: <2016-07-26 08:43:53 dan.harms>
rem Modified by: Dan Harms
rem Keywords: install perfect editor

@echo off
setlocal
setlocal enabledelayedexpansion


set verbose=
if not %1. == . (
    set verbose=v
)

set tar=c:\msys\1.0\bin\tar
set grep=c:\msys\1.0\bin\grep
set emacs=c:\emacs-24.5.1\bin\emacs
set user=%USERNAME%
set timeout=c:\Windows\System32\timeout
set orig_dir=%cd%
set script_dir=%~dp0
set int=emacs.tar
set manifest=.bk_manifest
set backup=emacs_bk.tar
set install_log=emacs-install.log

for /f "usebackq" %%i in (`hostname`) do set host=%%i

if "%HOME%". == . (
    echo "HOME directory undefined, aborting."
    exit /b
)

if exist %int% (
    del %int%
)

echo Tarring .emacs.d into %int%...
%tar% c%verbose%f %int% --exclude=*.elc .emacs.d
%tar% u%verbose%f %int% --transform=s$site/xr/$$ site/xr/.emacs.d
%tar% u%verbose%f %int% --transform=s$host/%host%/$$ host/%host%/.emacs.d

cd %HOME%
if exist .emacs.d (
    if exist %backup% del %backup%
    for /f %%i in (%orig_dir%\.emacs.d\%manifest%) do (
        call :backup %%i %backup%
    )
    %timeout% /t 2
    rmdir .emacs.d /s /q
    mkdir .emacs.d
    if exist %backup% (
        %tar% -xvpf %backup% --overwrite
        del %backup%
    )
)

echo Untarring %int% into %cd%...
%tar% -x%verbose%pf %orig_dir:C:=%\%int% --overwrite

del %orig_dir%\%int%

rem emacs will need forward slashes escaped, so double them
set path=%HOME:\=\\%\\.emacs.d
set cmd=(byte-recompile-directory \"%path%\" 0 t)

echo Compiling emacs files in directory %path%...
echo.
%emacs% --batch -u %user% --eval "%cmd%" > %install_log% 2>&1
%grep% -i error %install_log%
%grep% -e '^Done' %install_log%
echo.

rem %timeout% /t 5
pause

rem end main
exit /b %ERRORLEVEL%

rem function: call :func "arg1"
:backup
set file=%1
set tarfile=%2
if exist .emacs.d\%file% (
    echo Backing up %file%
    %tar% -r%verbose%f %tarfile% .emacs.d/%file%
)
exit /b 0

endlocal

rem xr-install-emacs.bat ends here
