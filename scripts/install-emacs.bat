@echo off
rem -*- Mode: dos -*-
rem xr-install-emacs.bat --- install emacs (xr style)
rem Copyright (C) 2015  Dan Harms (dan.harms)
rem Author: Dan Harms <dan.harms@xrtrading.com>
rem Created: Thursday, May 21, 2015
rem Version: 1.0
rem Modified Time-stamp: <2015-06-19 09:35:04 dan.harms>
rem Keywords: tar whole world

setlocal
setlocal enabledelayedexpansion


set verbose=
if not %1. == . (
    set verbose=v
)

set tar=c:\msys\1.0\bin\tar
set emacs=c:\emacs-24.4\bin\emacs
set user=%USERNAME%
set timeout=c:\Windows\System32\timeout
set curr_dir=%cd%
set script_dir=%~dp0
set int=emacs.tar
set dest=.emacs.d

echo Generating %curr_dir%\%int%...

if exist %int% (
    echo Removing existing %int%...
    del %int%
)
echo Tarring .emacs.d into %int%...
"%tar%" c%verbose%f %int% --exclude=*.elc --exclude=.git --exclude=.tags .emacs.d

if "%HOME%". == . (
    echo HOME directory undefined, aborting.
    exit /b
)

if exist "%HOME%\%dest%" (
    echo Removing "%HOME%\%dest%"...
    rmdir "%HOME%\%dest%" /s /q
)

echo Untarring %int% into "%HOME%"...
"%tar%" -C "%HOME%" -x%verbose%f %int%

rem emacs will need forward slashes escaped, so double them
set path=%HOME:\=\\%\\.emacs.d
set cmd=(byte-recompile-directory \"%path%\" 0 t)

%emacs% --batch -u %user% --eval "%cmd%"

echo ...Done.

%timeout% /t 2

endlocal

rem xr-install-emacs.bat ends here
