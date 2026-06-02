100 rem bit
110 b = $d020
120 print peek(b)
130 set bit b, 3
140 print peek(b)
150 getkey k$
160 clr bit b,3
170 print peek(b)
