10 rem * read sprite data
20 scnclr: sprite clr
30 for s1 = 1536 to 1598 : read q1 : poke s1,q1 : next
40 for s2 = 1599 to 1661 : read q2 : poke s2,q2 : next
50 for s3 = 1662 to 1723 : read q3 : poke s3,q3 : next
60 for s4 = 1724 to 1785 : read q4 : poke s4,q4 : next
70 rem * set sprite multi-colour
80 sprcolor 0,15 : rem * black and grey
90 rem * sprite 0 - joystick
100 sprite 0,1,2,,1,1,1
110 movspr 0,160,50
120 rem * sprite 1 - floppy disk
130 sprite 1,1,1,,1,1,1
140 movspr 1,160,96
150 rem * sprite 2 - cassette
160 sprite 2,1,1,,1,1,1
170 movspr 2,160,138
180 rem * sprite 3 - barrier
190 sprite 3,1,7,,1,1,1
200 movspr 3,160,168
210 rem * sprite data for joystick
220 data 0,2,192,0,10,176,0,10,176,0,10,176
230 data 0,10,160,0,2,128,0,1,64,0,1,64
240 data 0,1,64,0,1,64,0,1,64,0,3,192
250 data 0,5,112,10,197,80,21,85,84,255,255,255
260 data 213,85,87,85,85,85,85,85,87,85,85,85
270 data 21,85,92
280 rem * sprite data for floppy disk
290 data 0,0,0,255,255,87,106,170
300 data 85,106,170,85,106,170,84,106
310 data 170,85,85,85,85,85,85,85
320 data 85,235,85,85,130,85,85,130
330 data 85,85,130,85,85,235,85,85
340 data 85,85,85,85,85,85,125,85
350 data 85,125,85,213,125,87,85,125
360 data 85,213,125,87,244,85,31,129
370 rem * sprite data for cassette
380 data 0,0,0,0,0,0,0,0,0,85,85,85
390 data 127,255,253,255,255,255,170,170,170,245,85,95
400 data 149,191,86,223,109,247,155,109,182,219,109,183
410 data 149,85,86,229,85,91,191,255,254,247,255,223
420 data 219,125,231,93,190,117,85,85,85,0,0,0
430 data 0,0,0
440 rem * sprite data for barrier
450 data 0,0,0,0,0,0,0,0,0,0,0,0
460 data 0,0,0,0,0,0,0,0,0,85,85,85
470 data 90,86,149,105,90,85,101,105,89,85,165,105
480 data 86,149,165,85,85,85,0,0,0,0,0,0
490 data 0,0,0,0,0,0,0,0,0,0,0,0
500 data 0,0,0

    
