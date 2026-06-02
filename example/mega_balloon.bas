10 rem up, up and away
20 print "{clr}"
30 if peek(53272) and 32 then goto 50
40 poke 53295,asc("g"):poke 53295,asc("s")
50 rem setup sprite
60 ad = 4096
70 tc = 10
80 spr = peek(53356)+peek(53357)*256
90 poke spr, ad/64
100 for i=ad to ad + 62
110 poke i,tc
120 next i 
130 poke 53287, tc
140 rem poke 53248,100
150 rem poke 53249,100
160 poke 53269,1
170 for i = 0 to 62: read q: poke ad + i, q :next i
180 for x = 0 to 200
190 poke 53248,x : rem neue x-Koordinate
200 poke 53249,x : rem neue y-koordinate
210 next x
220 goto 180
230 end
240 data 0,127,0,1,255,192,3,255,224,3,231,224
250 data 7,217,240,7,223,240,7,217,240,3,231,224
260 data 3,255,224,3,255,224,2,255,160,1,127,64
270 data 1,62,64,0,156,128,0,156,128,0,73,0,0,73,0,0
280 data 62,0,0,62,0,0,62,0,0,28,0
