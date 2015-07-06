@echo off
setlocal
setlocal enabledelayedexpansion

set tar=c:\msys\1.0\bin\tar
set curr_dir=%cd%
set script_dir=%~dp0
set dest=config.tar

echo Generating %curr_dir%\%dest%...
%tar% cvf %dest% config doc .gitignore
%tar% uvf %dest% --transform=s/scripts/bin/ scripts
%tar% uvf %dest% --transform=s$bash/$$ bash
%tar% uvf %dest% --transform=s$tcsh/$$ tcsh
%tar% uvf %dest% --transform=s$xr/$$ xr
%tar% uvf %dest% --exclude=*.elc .emacs.d

echo ...done generating %dest%

endlocal
