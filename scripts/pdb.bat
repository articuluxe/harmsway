@echo off
rem -*- Mode: bat -*-
rem pdb.bat --- a pdb wrapper
rem Copyright (C) 2019-2020  Dan Harms (dan.harms)
rem Author: Dan Harms <enniomore@icloud.com>
rem Created: Tuesday, October 15, 2019
rem Version: 1.0
rem Modified Time-stamp: <2020-09-19 16:37:43 dharms>
rem Modified by: Dan Harms
rem Keywords: python tools debugging

setlocal
setlocal enabledelayedexpansion

python -u -m pdb %1

endlocal

rem code ends here
