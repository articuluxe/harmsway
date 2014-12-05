@echo off
setlocal
setlocal enabledelayedexpansion

set root=%~d0
set ctags=c:\ctags58\ctags
set c_loc="c:\Program Files (x86)\Microsoft Visual Studio 11.0\VC\include"
set qt_loc=c:\Qt\Qt5.2.1\5.2.1\Src
set c++-kinds=+l
rem unsupported c++-option on win32?  --file-scope=no
set c++-options="--c++-kinds=%c++-kinds%"
set c_file=c++-tags
set proto_file=rex-proto-tags
set qt_file=rex-qt-tags

rem analyze c++ standard library (if not present: delete to regenerate)
if not exist c++-tags (
   %ctags% -Re --language-force=c++ -h=".h.H.hh.hpp.hxx.h++.inc.def." %c++-options% -f %c_file% %c_loc%
)

rem analyze qt files (if not present: delete to regenerate)
if not exist rex-qt-tags (
   %ctags% -Re %c++-options% --exclude="*.js" -f %qt_file% %qt_loc%
)

rem analyze protobuf files
%ctags% -Re %c++-options% --langdef=pb --langmap=pb:.proto --regex-pb="/message[ \t]+([^ \t{]+)/\1/" -f %proto_file% protobuf_rex

rem generate local tags + aggregate all other tags
%ctags% -Re %c++-options% --etags-include=%proto_file% --etags-include=%qt_file% --etags-include=%c_file% --exclude=*moc*

endlocal
