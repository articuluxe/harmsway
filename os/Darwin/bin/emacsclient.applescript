tell application "Terminal"
	 try
		do shell script "/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -c -n &"
		tell application "Emacs" to activate
	on error
	   do shell script "/Applications/Emacs.app/Contents/MacOS/Emacs"
	end try
end tell

