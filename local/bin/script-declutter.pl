#!/usr/bin/perl -wp

# clean up control characters and other non-text detritus that shows up 
# when you run the "script" command.

BEGIN {
	# xterm titlebar escape sequence
	$xtermesc       = "\x1b\x5d\x30\x3b";

	# the occurence of a backspace event (e.g. cntrl H, cntrol W, or cntrl U)
	$backspaceevent = "\x1b\\\x5b\x4b"; # note escaping of third character

	# ANSI color escape sequence
	$ansiesc        = qr/\x1b\[[\d;]*?m/;

	# technically, this is arrow-right.  For some reason, being used against
	# very long backspace jobs.  I don't fully understand this, as evidenced
	# by the fact that is off by one sometimes.
	$bizarrebs     = qr/\x1b\[C/;

	# used as part of the xterm titlebar mechanism, or when
	# a bell sounds, which might happen when you backspace too much.
	$bell      = "\x07"; # could use \a

	$cr        = "\x0d"; # could use \r

	$backspace = "\x08"; # could use \b
}


s/$xtermesc.+?$bell//g;
s/[$cr$bell]//g;
s/${backspaceevent}//g;
s/$ansiesc//g;
while (s/(.)(?=$backspace)//) { s/$backspace//; } # frickin' sweet 
# For every ^H delete the character immediately left of it, then delete the ^H.
# Perl's RE's aren't R, so I wonder if I could do this in one expression.
while (s/(..)(?=$bizarrebs)//) { s/$bizarrebs//; }

# notes
# 	^[[7P has been spotted.  Based on http://www.google.com/codesearch/p?hl=en#4qbG402gtc0/myScreen.C&q="[7P" it appears to be a numbered cursor jump, moving 7 characters (not sure if left or right).
