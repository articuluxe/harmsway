on open location this_url
   do shell script "/Applications/Emacs.app/Contents/MacOS/bin/emacsclient \"" & this_url & "\""
   tell application "Emacs" to activate
end open location
