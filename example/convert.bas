10 print "{$e2}{left}{cbm-@}{$ee}{$83}{clr}{$ee}{cbm-o}{clr}"
20 print "enter a temperature in degrees fahrenheit:"
30 input f
40 c=(f-32)*5/9
50 print
60 print f;" degrees fahrenheit is ";c;" degrees celsius."
70 rem 
80 do: get a$: loop until a$ <> ""
90 print "{clr}"
100 goto 10
