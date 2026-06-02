10 screen 320,200,2
20 base = $4000
30 for i=0 to 63: read b: poke base+i,b: next
40 poke $07f8, base/64
50 sprite on 0
60 colorspr 0,2

70 xc = 160 : yc = 100
80 r = 60
90 a = 0

100 x = xc + r * cos(a)
110 y = yc + r * sin(a)
120 sprite 0,x,y
130 a = a + 0.05
140 goto 100

1000 data 0,24,60,126,255,255,255,255
1010 data 255,255,126,60,24,0,0,0
1020 data 0,0,24,60,126,255,255,255
1030 data 255,255,255,126,60,24,0,0
