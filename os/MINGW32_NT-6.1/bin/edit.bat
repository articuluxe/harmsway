@echo off
rem -*- Mode: bat -*-
rem edit.bat --- batch file for emacs gui on dos
rem Copyright (C) 2018  Dan Harms (dan.harms)
rem Author: Dan Harms <dan.harms@xrtrading.com>
rem Created: Monday, January 15, 2018
rem Version: 1.0
rem Modified Time-stamp: <2018-01-15 10:20:06 dan.harms>
rem Modified by: Dan Harms
rem Keywords: tools win32

setlocal
setlocal enabledelayedexpansion

%EDITOR% %*

endlocal

rem code ends here
