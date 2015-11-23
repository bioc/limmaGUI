#  UTILITY FUNCTIONS


LGchangeLog <- function(n=20)
#	Write first n lines of limmaGUI changelog
#	Gordon Smyth
#	20 Sep 2004.  Last modified 30 Oct 2004.
{
	writeLines(readLines(system.file("doc","changelog.txt",package="limmaGUI"),n=n))
}

