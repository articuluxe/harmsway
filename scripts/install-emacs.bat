@echo off
rem -*- Mode: bat -*-
rem install-emacs.bat --- install emacs (win32 style)
rem Copyright (C) 2015-2020  Dan Harms (dan.harms)
rem Author: Dan Harms <dan.harms@xrtrading.com>
rem Created: Thursday, May 21, 2015
rem Version: 1.0
rem Modified Time-stamp: <2020-09-08 23:37:24 dharms>
rem Modified by: Dan Harms
rem Keywords: install perfect editor

setlocal
setlocal enabledelayedexpansion


set verbose=
if not %1. == . (
    set verbose=v
)

if "%HOME%". == . (
    echo "HOME directory undefined, aborting."
    exit /b
)
if "%EMACS_DIR%". == . (
    echo "EMACS_DIR undefined, aborting."
    exit /b
)

if "%TAR_EXE%". == . (
   echo "TAR_EXE undefined, aborting."
   exit /b
   )

if "%GREP_EXE%". == . (
    echo "GREP_EXE undefined, aborting."
    exit /b
)

if "%FIND_EXE%". == . (
    echo "FIND_EXE undefined, aborting."
    exit /b
)

set "user=%USERNAME%"
set "timeout=c:\Windows\System32\timeout"
set orig_dir=%cd%
set script_dir=%~dp0
set int=emacs.tar
set manifest=.bk_manifest
set backup=emacs_bk.tar
set install_log=emacs-install.log

for /f "usebackq" %%i in (`hostname`) do set host=%%i


if exist %int% (
    del %int%
)

echo Tarring .emacs.d into %int%...
%TAR_EXE% c%verbose%f %int% --exclude=*.elc .emacs.d .fonts
%TAR_EXE% u%verbose%f %int% --transform=s$site/xr/$$ site/xr/.emacs.d
%TAR_EXE% u%verbose%f %int% --transform=s$ext$.emacs.d/ext$ ext
%TAR_EXE% u%verbose%f %int% --transform=s$host/%host%/$$ host/%host%/.emacs.d

cd "%HOME%"
if exist .emacs.d (
    if exist %backup% del %backup%
    "%FIND_EXE%" .emacs.d/backups -mindepth 1 -type d -empty -delete
    for /f %%i in (%orig_dir%\.emacs.d\%manifest%) do (
        call :backup %%i %backup%
    )
    "%timeout%" /t 2
    rmdir .emacs.d /s /q
    mkdir .emacs.d
    if exist %backup% (
        "%TAR_EXE%" -xvpf %backup% --overwrite
        rem del %backup%
    )
)

echo Untarring %int% into %cd%...
%TAR_EXE% -x%verbose%pf %orig_dir:C:=%\%int% --overwrite

rem del %orig_dir%\%int%

rem emacs will need forward slashes escaped, so double them
set path=%HOME:\=\\%\\.emacs.d
set cmd=(byte-recompile-directory \"%path%\" 0 t)

echo Compiling emacs files in directory %path%...
echo.
"%EMACS_DIR%\bin\emacs" --batch -u %user% --eval "%cmd%" > %install_log% 2>&1
"%GREP_EXE%" -i error %install_log%
"%GREP_EXE%" -e '^Done' %install_log%
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
    "%TAR_EXE%" -r%verbose%f %tarfile% .emacs.d/%file%
)
exit /b 0

endlocal

rem install-emacs.bat ends here
