@echo off
rem -*- Mode: bat -*-
rem pdb.bat --- a pdb wrapper
rem Copyright (C) 2019  Dan Harms (dan.harms)
rem Author: Dan Harms <dan.harms@xrtrading.com>
rem Created: Tuesday, October 15, 2019
rem Version: 1.0
rem Modified Time-stamp: <2019-10-15 11:50:50 dan.harms>
rem Modified by: Dan Harms
rem Keywords: python tools debugging

setlocal
setlocal enabledelayedexpansion

python -u -m pdb %1

endlocal

rem code ends here
