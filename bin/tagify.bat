@echo off
setlocal
setlocal enabledelayedexpansion

set curr_dir=%cd%
set root_dir=%~d0
set ctags=c:\ctags58\ctags
set c_loc="c:\Program Files (x86)\Microsoft Visual Studio 11.0\VC\include"
set qt_loc=c:\Qt\Qt5.2.1\5.2.1\Src
set c++-kinds=+l
rem unsupported c++-option on win32?
set c++-options=--file-scope=no --c++-kinds=%c++-kinds%
set c_file=c++-tags
set proto_file=rex-proto-tags
set qt_file=rex-qt-tags

rem analyze c++ standard library (if not present: delete to regenerate)
if not exist %c_file% (
   echo %c_file% not present, regenerating...
   %ctags% -Re --language-force=c++ -h=".h.H.hh.hpp.hxx.h++.inc.def." %c++-options% -f %c_file% %c_loc%
)

rem analyze qt files (if not present: delete to regenerate)
if not exist %qt_file% (
set temp=%curr_dir%\.%qt_file%
echo Generating Qt file name list in %temp%...
( dir /s /b %qt_loc%\*.h;%qt_loc%\*.cpp | findstr /v tests | findstr /v mkspecs | findstr /v 3rdparty | findstr /v examples | findstr /v qtwebkit | findstr /v qtdoc ) > %temp%
echo %qt_file% not present, regenerating...
%ctags% -e %c++-options% -f %qt_file% -L %temp%
)

rem analyze protobuf files
echo Generating %proto_file%
%ctags% -Re %c++-options% --langdef=pb --langmap=pb:.proto --regex-pb="/message[ \t]+([^ \t{]+)/\1/" -f %proto_file% protobuf_rex

rem generate local tags + aggregate all other tags
echo Generating TAGS...
%ctags% -Re %c++-options% --etags-include=%proto_file% --etags-include=%qt_file% --etags-include=%c_file% --exclude=*moc*

endlocal
