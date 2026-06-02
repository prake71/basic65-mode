10 rem  *******************************
20 rem  *                             *
30 rem  *       checksummer 64        *
40 rem  *                             *
50 rem  *       (version  2.0)        *
60 rem  *                             *
70 rem  *           64'er             *
80 rem  *                             *
90 rem  *        commodore 64         *
100 rem *                             *
110 rem *******************************
120 print"{clr}             {rvon}checksummer 64{rvof}"
130 print
140 sa=820:fori=satosa+6:reada:pokei,a:nexti
150 data133,95,134,96,76,191,163
160 poke88,0:poke89,192:poke90,0:poke91,192:poke780,0:poke781,160:syssa
170 poke88,0:poke89,0:poke90,0:poke91,0:poke780,0:poke781,224:syssa
180 poke1,53:poke42289,96:poke42290,228
190 fori=58464to58554:reada:pokei,a:nexti
200 print"{down}{down}{down}{down}         checksummer aktiviert."
210 print"{down}{down}ausschalten : poke1,55"
220 print"{down}anschalten  : poke1,53":new
230 data160,2,169,0,133,2,177,95
240 data240,15,201,32,208,3,200,208
250 data245,24,101,2,133,2,76,110
260 data228,192,4,48,241,198,214,165
270 data214,72,162,3,169,32,157,1
280 data4,189,183,228,32,210,255,202
290 data16,242,166,2,169,0,32,205
300 data189,169,62,32,210,255,104,133
310 data214,32,108,229,169,141,32,210
320 data255,76,128,164,92,72,32,201
330 data255,170,104,144,1,138,96,9
340 data60,18,19
    
