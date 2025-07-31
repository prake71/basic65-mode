
# A BASIC 65 mode for Emacs
As a retro nerd I want to have a cross development environment for
developing in BASIC for my MEGA65 computer. I want to use my
well-known editor, which is Emacs, for this. There is a MEGA65
emulator called [Xemu](https://github.com/xemu-project/xemu) available
which I want to use to run my code written in Emacs on.

Before the programming comes the research. So far I found some
excellent articles for cross-development for the MEGA65 by Dan
Sanderson who is very busy in the MEGA65 community:

<https://dansanderson.com/mega65/back-to-basics/>

<https://dansanderson.com/mega65/cross-development/>


This should be my test program for testing the workflow:

	100 print "enter a temperature in degrees fahrenheit:"
	110 input f
	120 c=(f-32)*5/9
	130 print
	140 print f;" degrees fahrenheit is ";c;" degrees celsius."

The mode for Emacs should at least consider

- Syntax highlighting for keywords, commands, functions
- Line numbers (as BASIC 65 is line-numbered)
- File association (.bas65, for example)
- converting the program code to a PRG file and run it on Xemu


