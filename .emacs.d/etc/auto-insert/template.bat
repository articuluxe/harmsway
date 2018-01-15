@echo off
rem -*- Mode: bat -*-
rem ${1:`(file-name-nondirectory (or buffer-file-name (buffer-name)))`} --- ${2:Short description}
rem Copyright (C) `(format-time-string "%Y")`  `(user-full-name)` (`(user-login-name)`)
rem Author: `(user-full-name)` <`user-mail-address`>
rem Created: `(insert-today)`
rem Version: ${3:1.0}
rem Modified Time-stamp: <2018-01-15 10:20:58 dan.harms>
rem Modified by: Dan Harms
rem Keywords: $4

setlocal
setlocal enabledelayedexpansion

$0

endlocal

rem code ends here
