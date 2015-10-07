rem -*- Mode: dos -*-
rem ${1:`(file-name-nondirectory (buffer-file-name))`} --- ${2:Short description}
rem Copyright (C) `(format-time-string "%Y")`  `(user-full-name)` (`(user-login-name)`)
rem Author: `(user-full-name)` <`user-mail-address`>
rem Created: `(insert-today)`
rem Version: ${3:1.0}
rem Modified Time-stamp: <2015-10-06 14:09:52 dan.harms>
rem Modified by: Dan Harms
rem Keywords: $4

@echo off
setlocal
setlocal enabledelayedexpansion

$0

endlocal

rem $1 ends here
