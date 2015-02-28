rem -*- Mode: dos -*-
rem ${1:`(file-name-nondirectory (buffer-file-name))`} --- ${2:Short description}
rem Copyright (C) `(format-time-string "%Y")`  `(user-full-name)` (`(user-login-name)`)
rem Author: `(user-full-name)` <`user-mail-address`>
rem Created: `(today)`
rem Version: ${3:1.0}
rem Modified Time-stamp: <2015-02-27 03:54:35 dharms>
rem Keywords: $4

@echo off
setlocal
setlocal enabledelayedexpansion

$0

endlocal

rem $1 ends here
