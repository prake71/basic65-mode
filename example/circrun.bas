10 rem *** buchstabe im kreis ***
20 x0 = 40 : y0 = 12        : rem mittelpunkt
30 r  = 10                  : rem radius
40 a  = 0                   : rem startwinkel
50 c = asc("#")             : rem buchstabe
60 rem bildschirm löschen
70 print chr$(147)
80 rem endlosschleife
90 x = x0 + r * cos(a)
100 y = y0 + r * sin(a)
110 rem alten bildschirm löschen
120 print chr$(147)
130 rem buchstaben setzen
140 t@&(x,y) = 35
150 a = a + 0.1:sleep 0.1
155 if a > 359 then a = 0
160 goto 90
