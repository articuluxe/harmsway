@echo off
setlocal
setlocal enabledelayedexpansion

set files=
set edit=runemacs.exe

rem gather gui files
for /f "usebackq" %%i in (`dir /b /s *.cpp *.h ^| findstr /v moc ^| findstr /v qrc`) do (
set files=!files! %%i
)

rem gather protobuf files
for /f "usebackq" %%i in (`dir /b /s ..\..\protobuf_rex\protobuf_rex\*.proto`) do(
set files=!files! %%i
)

rem gather any python scripts
for /f "usebackq" %%i in (`dir /b /s ..\..\..\utility_scripts\*.py`) do (
set files=!files! %%i
)

rem gather build scripts
for /f "usebackq" %%i in (`dir /b /s ..\..\..\*.bat`) do (
set files=!files! %%i
)

%edit% %files%

endlocal
