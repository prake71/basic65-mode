10 rem up, up and away
20 print "{clr}"
30 v=53248    : rem Basisadresse des VIC
40 poke v+21,4 : rem Sprite 2 aktivieren
50 poke 2042,13: rem Daten für Sprite 2 aus Block 13 (13*64)
60 for n=0 to 62 : read q:poke 832+n,q:next : rem sprite daten 
70 for x = 0 to 200
80     poke v+4,x : rem neue x-Koordinate
90     poke v+5,x : rem neue y-koordinate
100 next x
110 goto 70
120 data 0,127,0,1,255,192,3,255,224,3,231,224
130 data 7,217,240,7,223,240,7,217,240,3,231,224
140 data 3,255,224,3,255,224,2,255,160,1,127,64
150 data 1,62,64,0,156,128,0,156,128,0,73,0,0,73,0,0
160 data 62,0,0,62,0,0,62,0,0,28,0
170 print "{clr}"
