rem -*- Mode: dos -*-
rem start-pageant.bat --- start pageant on start up
rem Copyright (C) 2015   (dan.harms)
rem Author:  <dan.harms@xrtrading.com>
rem Created: Friday, April 10, 2015
rem Version: 1.0
rem Modified Time-stamp: <2015-04-13 09:39:20 dan.harms>
rem Keywords: pageant ssh

@echo off
setlocal
setlocal enabledelayedexpansion

set exe="c:\Program Files\Portable PuTTY\pageant.exe"
set key_dir="c:\Users\dan.harms.XRTRADING\.ssh"
set key_file=Dan.Harms.ppk

start %exe% %key_dir%\%key_file%

endlocal

rem start-pageant.bat ends here
