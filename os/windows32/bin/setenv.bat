@echo off
rem -*- Mode: bat -*-
rem setenv.bat --- set environment variables
rem Copyright (C) 2019  Dan Harms (Dan.Harms)
rem Author: Dan Harms <dan.harms@xrtrading.com>
rem Created: Thursday, December  5, 2019
rem Version: 1.0
rem Modified Time-stamp: <2019-12-16 08:57:07 Dan.Harms>
rem Modified by: Dan.Harms
rem Keywords:

rem user variables

echo "Setting environment variables..."

setx NAME Dan.Harms
setx EMACS_ROOT "C:\Program Files\emacs-26.3"
setx EMACS_BIN "%EMACS_ROOT%\bin"
setx EDITOR "%EMACS_BIN%\runemacs.exe"
setx VISUAL "%EMACS_BIN%\emacs.exe"
setx MSYS_DIR "C:\Program Files (x86)\GnuWin32\bin"
setx GIT_DIR "C:\Program Files\Git"
setx FIND_EXE "%GIT_DIR%\usr\bin\find"
setx GREP_EXE "%GIT_DIR%\usr\bin\grep"
setx HOSTNAME_EXE "%MSYS_DIR%\hostname"
setx TAR_EXE "%GIT_DIR%\usr\bin\tar"
setx PUTTY_DIR "%ChocolateyInstall%\bin"
setx DICPATH "%HOME%\config\hunspell"
setx DICTIONARY "%DICPATH%\ENU"

echo "Creating symbolic links..."

mklink "%ChocolateyInstall%\bin\git.exe" "%GIT_DIR%\bin\git.exe"

echo "...done."

rem code ends here
