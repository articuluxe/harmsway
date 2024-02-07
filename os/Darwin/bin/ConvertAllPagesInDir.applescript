on open dropped
	repeat with x from 1 to count of dropped
		set curritem to item x of dropped
		tell application "Finder"
			set isfolder to folder (curritem as string) exists
		end tell
		if isfolder = true then
			processFolder(curritem)
		end if
	end repeat
end open

on processFolder(curr)
	tell application "Finder"
		set currfiles to every file of curr whose name extension is "pages"
	end tell
	set itemcount to (get count of items in currfiles)
	repeat with x from 1 to itemcount
		set currfile to item x of currfiles
		processFile(currfile, curr)
	end repeat
end processFolder

on processFile(currfile, currdir)
	set currname to name of currfile
	set moddate to modification date of currfile
	--	display dialog "moddate: " & moddate
	set todaystr to formatDate(current date)
	set thenstr to formatDate(moddate)
	--	display dialog "today: " & todaystr & return & "then: " & thenstr
	set newtxtname to thenstr & "-" & text 1 thru -7 of currname & ".txt"
	set newpdfname to thenstr & "-" & text 1 thru -7 of currname & ".pdf"
	set currpath to (currdir as Unicode text) & currname
	set subdir to (currdir as text) & ".converted-at-" & todaystr
	tell application "Finder"
		if (exists folder subdir) = false then
			set subdir to make new folder at (currdir as text) with properties {name:".converted-at-" & todaystr}
		else
			set subdir to subdir & ":"
		end if
	end tell
	set newpath to (subdir as Unicode text)
	tell application "Pages"
		set currdoc to open file currpath
		export currdoc to file (newpath & newtxtname) as unformatted text
		export currdoc to file (newpath & newpdfname) as PDF
		close currdoc saving no
	end tell
end processFile

on formatDate(currdate)
	set y to text -4 thru -1 of ("0000" & (year of currdate))
	set m to text -2 thru -1 of ("00" & ((month of currdate) as integer))
	set d to text -2 thru -1 of ("00" & (day of currdate))
	return y & m & d
end formatDate
