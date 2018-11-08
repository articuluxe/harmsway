@echo off
rem -*- Mode: bat -*-
rem untar-world.bat --- untar the world (xr style)
rem Copyright (C) 2017-2018  Dan Harms (dan.harms)
rem Author: Dan Harms <dan.harms@xrtrading.com>
rem Created: Thursday, December 21, 2017
rem Version: 1.0
rem Modified Time-stamp: <2018-11-08 09:29:31 dan.harms>
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
if "%EMACS%". == . (
    echo "EMACS directory undefined, aborting."
    exit /b
)
if "%MSYS%". == . (
    echo "MSYS directory undefined, aborting."
    exit /b
)

set msys="%MSYS%\bin"
set emacs="%EMACS%\bin\emacs"

set tar="%msys%\tar"
set grep="%msys%\grep"
set user=%USERNAME%
set timeout=c:\Windows\System32\timeout
set orig_dir=%cd%
set script_dir=%~dp0
set int=world.tar
set manifest=.bk_manifest
set backup=emacs_bk.tar
set install_log=emacs-install.log

cd "%HOME%"

if exist .emacs.d (
    if exist %backup% del %backup%
    find .emacs.d\backups -mindepth 1 -type d -empty -delete
    for /f %%i in (%orig_dir%\.emacs.d\%manifest%) do (
        call :backup %%i %backup%
    )
    %timeout% /t 2
    rmdir .emacs.d /s /q
    mkdir .emacs.d
    if exist %backup% (
        %tar% -xvpf %backup% --overwrite
        rem del %backup%
    )
)

echo Untarring %int% into %cd%...
%tar% -x%verbose%pf %orig_dir:C:=%\%int% --overwrite

rem del %orig_dir%\%int%

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

rem untar-world.bat ends here
