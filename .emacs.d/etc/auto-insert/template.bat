@echo off
rem -*- Mode: dos -*-
rem ${1:`(file-name-nondirectory (buffer-file-name))`} --- ${2:Short description}
rem Copyright (C) `(format-time-string "%Y")`  `(user-full-name)` (`(user-login-name)`)
rem Author: `(user-full-name)` <`user-mail-address`>
rem Created: `(insert-today)`
rem Version: ${3:1.0}
rem Modified Time-stamp: <2015-06-18 13:42:57 dan.harms>
rem Modified by:
rem Keywords: $4

setlocal
setlocal enabledelayedexpansion

$0

endlocal

rem $1 ends here
