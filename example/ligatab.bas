1 poke53280,0:poke53281,0:printchr$(14):rem poke788,52:poke792,193
3 printchr$(147):print
4 print "        bitte warten"
5 gosub 655
6 print zd$zh$
7 print zh$"     "za$"                           "zb$"
8 print"     "za$" "zb$"                         "za$" "zb$
9 print "     "za$" "zb$"      l{$a0}i{$a0}g{$a0}a{$a0}t{$a0}a{$a0}b{$a0}     "za$" "zb$
10 print"     "za$" "zb$"                         "za$" "zb$
11 print"     "za$" "zb$"  by heuser franz-josef  "za$" "zb$
12 print"     "za$" "zb$"                         "za$" "zb$
13 print"     "za$" "zb$"  in den atzenbenden 74  "za$" "zb$
14 print"     "za$" "zb$"                         "za$" "zb$
15 print"     "za$" "zb$"    5100 aachen-haaren   "za$" "zb$
16 print"     "za$" "zb$"                         "za$" "zb$
17 print"     "za$" "zb$"    telefon 0241/16737   "za$" "zb$
18 print"     "za$" "zb$"                         "za$" "zb$
19 print"     "za$"                           "zb$"
20 print:print:print"     bitte geben sie das heutige"
21 print:print"     datum in der form tt.mm.jjjj"          "
22 print"                       ^^ ^^ ^^^^"          "
23 print"     ein.              ..........";
24 printze$ze$ze$ze$ze$ze$ze$ze$ze$ze$ze$ze$;:inputda$
25 print:print:printza$z0$zb$
26 gosub709
27 ifw$<>"j"then6
28 dimm$(20),m(20),d(38,10),e(38,10),l(20,9),g$(38,20),sr$(20),z(20)
29 dimh$(38,20),k1(20,2),ds$(38),n$(38,10),dn$(38,10)
30 gosub575
31 printzd$za$z1$zb$:print
32 print"-------- "zz$" ----------" :print
33 print"   "za$" 1 "zb$" = "zw$
34 print"   "za$" 2 "zb$" = "zt$
35 print"   "za$" 3 "zb$" = "zs$:print
36 print"--------    "zy$"   ----------":print
37 print"   "za$" 4 "zb$" = "zr$
38 print"   "za$" 5 "zb$" = "zq$
39 print"   "za$" 6 "zb$" = "zp$:print
40 print"--------   "zx$"   ----------":print
41 print"   "za$" 7 "zb$" = "zo$
42 print"   "za$" 8 "zb$" = "zn$:print
43 print"{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}":print
44 print"   "za$" 9 "zb$" = "za$zm$zb$
45 print:printzl$
46 gosub709
47 w=val(w$):ifw<1orw>9then46
48 onwgosub50,88,147,194,219,291,331,374,411
49 goto31
50 printzd$:printza$z2$zb$:print
51 ifln$=""thenln$="................"
52 ifa=0thenprint:print"        e{$a0}r{$a0}s{$a0}t{$a0}e{$a0}i{$a0}n{$a0}g{$a0}a{$a0}b{$a0}e{$a0}{$a0} ":print:print
53 ifa=0thenprint" liganame =   ";ln$:printzc$zg$zg$zg$zg$zg$
54 ifa=0thenprint:forx=1to12:printzf$;:nextx
55 ifa=0theninputln$:ln$=left$(ln$,16):print
56 ifa=0theninput" wieviele mannschaften (3-20) ";a:gosub571:y=1
57 ifa<3ora>20thena=0:goto50
58 printzd$za$z2$zb$:print
59 fori=1toa
60 ifi<10thenprintzf$;
61 printi;ze$;". mannschaft     ";m$(i)
62 nexti
63 ify=1thengosub82
64 printzg$za$z0$zb$
65 gosub709
66 ifw$="j"theny=0:return
67 ifw$<>"n"then65
68 printzh$;:print"        "za$"n"zb$"ame  "za$"a"zb$"nzahl falsch ?";
69 print"          "
70 gosub709
71 ifw$="n"theny=1:gosub82:goto80
72 ifw$="a"thenprintzh$zh$
73 print"        "za$"-"zb$" = eine mannschaft weniger"
74 print:print"        "za$"+"zb$" = eine mannschaft mehr"
75 gosub709
76 ifw$="-"thengosub497:y=0:goto58
77 ifw$="+"thengosub526:y=0:goto58
78 goto75
79 w$="a"
80 ifw$="n"thenprintzh$:goto64
81 ifw$<>"n"orw$<>"a"then70
82 printzc$zg$
83 fori=1toa
84 forx=1to18:printzf$;:nextx
85 inputm$:m$(i)=left$(m$,15):m$=""
86 nexti
87 return
88 printzd$za$z3$zb$:print:print
89 print"   "za$" e "zb$" = spielplan       eingeben":print
90 print"   "za$" s "zb$" = spiele          sichten":print
91 print"         ergebnisse      -e-s-k-":print
92 print"   "za$" n "zb$" = nachholspiele   -e-s-k-":print
93 print"   "za$" m "zb$" = menue"
94 gosub709
95 ifw$="m"thenreturn
96 ifw$="e"then101
97 ifw$="s"thenprint:print:print"   start bei spieltag (1-";b;")";:inputbs
98 ifw$="s"then120
99 ifw$="n"then685
100 goto88
101 gosub571
102 gosub477
103 fori=1toint(b/2)
104 gosub712
105 printza$i;ze$;"tag wer spielte gegen wen (1 -"a;:printze$")   "zb$:print
106 forj=1toc
107 printzf$;d(i,j)
108 printzh$zf$zf$zf$zf$zf$" - ";:print e(i,j):printzh$zh$
109 inputd$
110 d(i,j)=val(left$(d$,2)):d$=""
111 printzh$zf$zf$zf$zf$" - ";:input e$:e(i,j)=val(left$(e$,2)):e$=""
112 nextj
113 printza$z0$zb$
114 gosub709
115 ifw$="m"thenreturn
116 ifw$<>"j"then104
117 nexti
118 gosub519
119 goto88
120 gosub571:ifbs<1orbs>bthen88
121 fori=bstob
122 gosub712
123 printzc$:forx=1to6:printzg$:nextx
124 printza$i;ze$;"tag       e{$a0}r{$a0}g{$a0}e{$a0}b{$a0}n{$a0}i{$a0}s{$a0}s{$a0}e       "zb$:print
125 forj=1toc
126 gosub732:printd(i,j)
127 printzh$zf$zf$zf$zf$" - ";:printe(i,j):printzh$zh$
128 ifn$(i,j)="n"thenprint:printzh$;:for x=1to18:printzf$;:nextx
129 ifn$(i,j)="n"thenprint" nachholspiel":goto137
130 ifg$(i,d(i,j))<>""thenprint:printzh$;:forx=1to18:printzf$;:nextx
131 ifg$(i,d(i,j))<>""thenprintg$(i,d(i,j));
132 ifg$(i,d(i,j))<>""thenprint:printzh$;:forx=1to23:printzf$;:nextx:print"*";
133 ifh$(i,d(i,j))<>""thenprint"  ";h$(i,d(i,j))
134 ifg$(i,d(i,j))=""andh$(i,d(i,j))=""thenprint:printzh$;
135 ifg$(i,d(i,j))=""andh$(i,d(i,j))=""thenprint;:forx=1to23:printzf$;:nextx
136 ifg$(i,d(i,j))=""andh$(i,d(i,j))=""thenprint"*"
137 gosub735
138 nextj
139 ifa<19thenprint
140 printza$zk$zb$:gosub709
141 ifw$=zh$theni=i:ifi=btheni=0
142 ifw$=zg$theni=i-2:ifi<0theni=b-1
143 ifw$="k"thengosub552
144 ifw$="m"thenreturn
145 nexti
146 goto88
147 gosub571
148 printzd$za$z4$zb$:print
149 print"      bis zu welchem spieltag ?"
150 ifb=0thenreturn
151 print:print"      1 - ";b;:input k
152 ifk<1ork>bthen148
153 gosub 738:print:print"      ich berechne die tabelle !"
154 print:print"             bitte warten "
155 fori=1to20
156 forj=1to9
157 l(i,j)=0
158 nextj
159 nexti
160 fori=1toa
161 forj=1tok
162 ifg$(j,i)=""orh$(j,i)=""then169
163 l(i,4)=l(i,4)+1
164 l(i,1)=l(i,1)+val(g$(j,i))
165 l(i,2)=l(i,2)+val(h$(j,i))
166 ifval(g$(j,i))>val(h$(j,i))thenl(i,5)=l(i,5)+1
167 ifval(g$(j,i))=val(h$(j,i))thenl(i,6)=l(i,6)+1
168 ifval(g$(j,i))<val(h$(j,i))thenl(i,7)=l(i,7)+1
169 nextj
170 nexti
171 fori=1toa
172 l(i,3)=l(i,1)-l(i,2)
173 l(i,8)=(l(i,5)*2)+l(i,6)
174 l(i,9)=(l(i,7)*2)+l(i,6)
175 nexti
176 gosub622:gosub 739
177 ifr1=1thenr1=0:return
178 gosub 738:printzd$za$z4$zb$:print
179 print"    tabelle einschl.";k;ze$". spieltag":print:print
180 ifa>17thenprintzh$zh$
181 print"pl.{shift--} name          {shift--} tord. {shift--} +pu.{shift--} -pu."
182 print"{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}":ty=0
183 fori=1toa
184 ifm$(z(i))="..............."then190
185 ty=ty+1:printty;
186 print:printzh$zf$zf$zf$;left$(m$(z(i)),15)
187 printzh$;:forx=1to21:printzf$;:nextx:print l(z(i),3)
188 printzh$;:forx=1to28:printzf$;:nextx:print l(z(i),8)
189 printzh$;:forx=1to34:printzf$;:nextx:print l(z(i),9)
190 nexti
191 gosub739:ifr1=1thenr1=0:return
192 gosub709
193 return
194 printzd$za$z5$zb$:print:print
195 print"   druckerausgabe mannschaftsliste":print
196 print"        stand :      ";da$:print
197 print"         drucker eingeschaltet ?":print
198 print"         wenn fertig dann ";za$zv$zb$
199 print:print:print "               m = menue"
200 gosub709
201 ifw$="m"then return
202 ifw$<>chr$(13)then194
203 close2:open2,4,7
204 print#2
205 print#2,chr$(14);"********** mannschaftsliste ***********";chr$(15)
206 print#2
207 print#2,chr$(14);ln$"     stand : ";da$;chr$(15)
208 print#2:gosub711
209 print#2,chr$(14);"{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}";chr$(15)
210 print#2
211 fori=1toastep4
212 print#2,m$(i);"      ";
213 print#2,m$(i+1);"      ";
214 print#2,m$(i+2);"     ";
215 print#2,m$(i+3)
216 nexti
217 print#2:print#2:print#2:close2
218 return
219 printzd$za$z6$zb$:print:print:print:print
220 print" "za$" a "zb$" = kompletter spielplan":print
221 print" "za$" b "zb$" = ein bestimmter spieltag":print
222 print" "za$" c "zb$" = von / bis zu einen spieltag ":print
223 print" "za$" d "zb$" = ergebnisse einer mannschaft":print
224 print" "za$" n "zb$" = nachholspiele":print
225 print" "za$" m "zb$" = menue":print:print:print
226 gosub709
227 ifw$="m"thenreturn
228 ifw$="a"thenb1=1:b2=b
229 ifw$="a"thenb3=1:goto240
230 ifw$="b"thenprint" welcher spieltag 1 - ";ze$;b;:inputb1:b2=b1
231 ifw$="b"thenb3=2:goto240
232 ifw$="c"orw$="n"thenprint" von welchem spieltag 1 - ";ze$;b;:inputb1:print
233 ifw$="c"orw$="n"thenprint" bis welchem spieltag";b1;"- ";ze$;b;:inputb2
234 b4=0:ifw$="n"thenb4=1
235 ifw$="c"orw$="n"thenb3=3:goto240
236 ifw$="d"thengosub435
237 goto219
238 ifb1<1orb1>bthen219
239 ifb2<b1orb2>bthen219
240 printzd$za$z6$zb$:print:print:print
241 print"           druckerausgabe":print:print
242 ifw$="a"thenprint "        kompletter spielplan"
243 ifw$="b"thenprint "           ";b1;ze$;". spieltag"
244 ifw$="n"thenprint"           nachholspiele":print:print
245 ifw$="c"orw$="n"thenprint "   vom";b1;"bis einschl.";b2;ze$;". spieltag"
246 print:print:print"        stand :   ";da$:print:print
247 print"      "za$" m "zb$" = menue "za$" d "zb$" = drucken"
248 gosub709
249 ifw$="m"thenreturn
250 ifw$<>"d"then219
251 gosub571
252 gosub 738:close2:open2,4,7
253 print#2,chr$(14);"********** s p i e l p l a n **********"chr$(15)
254 print#2
255 print#2,chr$(14);ln$;"      ";"stand :";da$;chr$(15)
256 ifb3=1then261
257 print#2
258 ifb3=3thenprint#2,chr$(14);"   vom";b1;". bis einschl.";
259 ifb3=3thenprint#2,b2;". spieltag";chr$(15)
260 ifb3=2thenprint#2,chr$(14);"             ";b2;". spieltag";chr$(15)
261 print#2:gosub 711
262 print#2,chr$(14);"{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}";chr$(15)
263 print#2:ifb4=1thengoto425
264 fori=b1tob2step2
265 print#2,"*";i;". spieltag ";ds$(i);
266 ifi+1>b2thenprint#2
267 ifi+1<=b2thenprint#2,chr$(16)chr$(52)chr$(48);"*";i+1;
268 ifi+1<=b2thenprint#2,". spieltag ";ds$(i+1)
269 print#2
270 j=1:x=1
271 foryy=jtoc
272 ifd(i,yy)=0thenj=j+1
273 ifd(i,yy)>0then275
274 nextyy
275 print#2,m$(d(i,j));"-";m$(e(i,j));" ";g$(i,d(i,j));":";h$(i,d(i,j));
276 j=j+1
277 ifb2=b1thenprint#2:goto284
278 forxx=xtoc
279 ifd(i+1,xx)=0thenx=x+1
280 ifd(i+1,xx)>0then282
281 nextxx
282 print#2,chr$(16)chr$(52)chr$(48);m$(d(i+1,x));"-";m$(e(i+1,x));" ";
283 print#2,g$(i+1,d(i+1,x));":";h$(i+1,d(i+1,x))
284 x=x+1
285 ifx>cthen287
286 goto271
287 print#2
288 nexti
289 print#2:print#2:print#2:close2:gosub739
290 goto219
291 printzd$za$z7$zb$:print
292 r1=1:gosub149
293 printzd$za$z7$zb$:print
294 r1=1:gosub179
295 print:print"   "za$" d "zb$" =  drucken  "za$" m "zb$" = menue"
296 gosub709
297 ifw$="m"thenreturn
298 ifw$<>"d"then296
299 gosub 738:close2:open2,4,7
300 print#2
301 print#2,chr$(14);"************ t a b e l l e ************"
302 print#2
303 print#2,ln$;"     stand : ";da$
304 print#2
305 print#2,"        einschl.";k;". spieltag";chr$(15)
306 print#2:gosub 711
307 print#2,chr$(14);"{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}";chr$(15)
308 print#2
309 print#2
310 print#2,"       pl.   verein          sp.   g   u   v    +t  ";
311 print#2," -t   td    +p   -p  "
312 print#2,"      {cbm-q}{shift-*}{shift-*}{shift-*}{shift-+}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-+}{shift-*}{shift-*}{shift-*}{shift-*}{shift-+}{shift-*}{shift-*}{shift-*}{shift-*}{shift-+}{shift-*}{shift-*}{shift-*}{shift-+}{shift-*}{shift-*}{shift-*}{shift-+}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-+}";
313 print#2,"{shift-*}{shift-*}{shift-*}{shift-*}{shift-+}{shift-*}{shift-*}{shift-*}{shift-*}{shift-+}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-+}{shift-*}{shift-*}{shift-*}{shift-*}{cbm-w}":ty=0
314 fori=1toa
315 ifm$(z(i))="..............."then327
316 ty=ty+1:print#2,chr$(16)chr$(48)chr$(55);ty;
317 print#2,chr$(16)chr$(49)chr$(50);m$(z(i));
318 print#2,chr$(16)chr$(50)chr$(56);l(z(i),4);
319 print#2,chr$(16)chr$(51)chr$(52);l(z(i),5);
320 print#2,chr$(16)chr$(51)chr$(56);l(z(i),6);
321 print#2,chr$(16)chr$(52)chr$(50);l(z(i),7);
322 print#2,chr$(16)chr$(52)chr$(55);l(z(i),1);
323 print#2,chr$(16)chr$(53)chr$(50);l(z(i),2);
324 print#2,chr$(16)chr$(53)chr$(55);l(z(i),3);
325 print#2,chr$(16)chr$(54)chr$(51);l(z(i),8);
326 print#2,chr$(16)chr$(54)chr$(56);l(z(i),9)
327 nexti
328 print#2
329 print#2:print#2:print#2:close2:gosub739
330 printzh$zh$zh$:goto295
331 printzd$za$z8$zb$:print:print:print
332 print"      richtige diskette einlegen":print
333 ifln$=""thenln$="................"
334 gosub709
335 gosub740
336 print:print"      dateiname =   ";ln$
337 printzh$zf$zf$zf$zf$zf$zf$zf$zf$zf$zf$zf$zf$zf$zf$zf$zf$zf$zf$;:inputln$
338 print:printza$z0$zb$:gosub709
339 ifw$<>"j"then331
340 print:print"      wenn fertig dann ";za$zv$zb$:print
341 print"              m = menue"
342 gosub709
343 ifw$="m"thenreturn
344 ifw$<>chr$(13)then331
345 printzd$za$z8$zb$:print:print:print
346 print"   daten werden von diskette geladen"
347 print:print"            bitte warten":print
348 print:print"           ";za$"  l{$a0}a d e n  "zb$:print
349 gosub 738:close1:open1,8,3,ln$+",s,r"
350 input#1,a:gosub571
351 input#1,ln$
352 fori=1toa
353 input#1,m$(i)
354 nexti
355 fori=1tob
356 input#1,ds$(i)
357 forj=1toc
358 input#1,d(i,j)
359 input#1,e(i,j)
360 input#1,n$(i,j)
361 input#1,dn$:ifdn$<>"......198."thendn$(i,j)=dn$
362 nextj
363 nexti
364 fori=1tob
365 forj=1toa
366 input#1,g$(i,j)
367 input#1,h$(i,j)
368 nextj
369 nexti
370 close1:gosub739
371 gosub604
372 gosub571
373 return
374 printzd$za$z9$zb$:print:print:print
375 print"      richtige diskette einlegen":print
376 print "      dateiname = ";ln$:print
377 print "      wenn fertig dann ";za$zv$zb$:print
378 print "              m = menue"
379 gosub 709
380 if w$="m" then return
381 ifw$<>chr$(13)then374
382 printzd$za$z9$zb$:print:print:print
383 print" daten werden auf diskette gespeichert"
384 print:print"            bitte warten":print
385 gosub 587
386 print:print"           ";za$"  speichern  "zb$:print
387 gosub 738:close1:open1,8,3,"@:"+ln$+",s,w"
388 print#1,a;chr$(13)
389 print#1,ln$;chr$(13)
390 fori=1toa
391 print#1,m$(i);chr$(13)
392 nexti
393 fori=1tob
394 print#1,ds$(i);chr$(13)
395 forj=1toc
396 print#1,d(i,j);chr$(13)
397 print#1,e(i,j);chr$(13)
398 print#1,n$(i,j);chr$(13)
399 print#1,dn$(i,j);chr$(13)
400 nextj
401 nexti
402 fori=1tob
403 forj=1toa
404 print#1,g$(i,j);chr$(13)
405 print#1,h$(i,j);chr$(13)
406 nextj
407 nexti
408 close1:gosub 739
409 gosub604
410 return
411 printzd$za$y9$zb$:print:print
412 print"{$a0}   sind alle daten auf diskette":print
413 print"    gesichert  ??????????  (j/n)":print
414 input"                ";w$:print
415 ifw$<>"j"thenreturn
416 print"    ";za$" w{$a0}i{$a0}r{$a0}k{$a0}l{$a0}i{$a0}c{$a0}h{$a0} ???? "zb$;
417 input"  ";w1$
418 ifw1$<>"j"thenreturn
419 print:print:print:wx$=""
420 print"    n = neue liga":print
421 print"    e = programmende"
422 get wx$:ifwx$=""then422
423 ifwx$<>"e"thenclr:goto3
424 sys64738
425 print#2,chr$(14);" n a c h h o l s p i e l e ";chr$(15):print#2
426 fori=b1tob2
427 forj=1toc
428 ifn$(i,j)<>"n"then432
429 print#2,"vom";:ifi<10thenprint#2," ";
430 print#2,i;".spieltag ";ds$(i);" ";m$(d(i,j));" - ";
431 print#2,m$(e(i,j));" neu am ";dn$(i,j)
432 nextj
433 nexti
434 goto289
435 gosub571
436 printzd$za$z6$zb$:print
437 print"           druckerausgabe":print:print
438 print"     spielplan einer mannschaft"
439 print:print"     stand :         ";da$:print
440 print"     welcher manns.  ( 1 - ";ze$;a;ze$;" ) ";:inputmn:print
441 ifmn<1ormn>athenprintzh$zh$zh$:goto440
442 print"          ";m$(mn):print
443 printza$z0$zb$
444 gosub709
445 ifw$<>"j"then435
446 printzh$zh$
447 print"      "za$" m "zb$" = menue "za$" d "zb$" = drucken        "
448 gosub709
449 ifw$="m"thenreturn
450 ifw$<>"d"then448
451 close2:open2,4,7
452 print#2,chr$(14);"****** spielplan ";m$(mn);" ******";chr$(15):print#2
453 print#2,chr$(14);ln$"     stand : ";da$;chr$(15)
454 print#2:gosub711
455 print#2,chr$(14);"{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}{shift-*}";chr$(15)
456 print#2
457 fori=1tob
458 forj=1toc
459 ifm$(mn)<>m$(d(i,j))andm$(mn)<>m$(e(i,j))then462
460 print#2,i;".";chr$(16)chr$(48)chr$(55);ds$(i);"    ..... uhr     ";
461 print#2,m$(d(i,j));" {shift-*} ";m$(e(i,j));"  ";g$(i,d(i,j));":"h$(i,d(i,j))
462 nextj
463 nexti
464 print#2:print#2:print#2:close2
465 print:print:print" moechten sie einen weiteren ausdruck"
466 print:print" von mannschaft       ";m$(mn)     "
467 gosub709
468 ifw$="j"then451
469 ifw$<>"n"then467
470 printzh$zh$zh$zh$
471 print" moechten  sie  einen  ausdruck einer  "
472 print:print" anderen  mannschaft          ( j/n )  "
473 gosub709
474 ifw$="j"then435
475 ifw$<>"n"then473
476 return
477 printzd$za$z3$zb$:print
478 print:print"        bitte datum in der form":print
479 print"        > tt.mm.jjjj < eingeben":print
480 fori=1tobstep2
481 printi;ze$;". ":printzh$zf$zf$zf$zf$zf$zf$;ds$(i)
482 printzh$;:forx=1to20:printzf$;:nextx:print i+1;ze$;". "
483 printzh$;:forx=1to27:printzf$;:nextx:printds$(i+1)
484 nexti
485 print:printza$z0$zb$
486 gosub709
487 ifw$="j"thenreturn
488 ifw$<>"n"then486
489 printzc$zg$zg$zg$zg$zg$zg$
490 ifa=17ora=18thenprintzh$zh$zh$
491 ifa=19ora=20thenprintzh$zh$zh$zh$zh$
492 fori=1tobstep2
493 printzf$zf$zf$zf$;:inputds$:ds$(i)=left$(ds$,10):printzh$;
494 forx=1to25:printzf$;:nextx:inputds$:ds$(i+1)=left$(ds$,10)
495 nexti
496 goto485
497 foram=1toa
498 ifleft$(m$(am),5)="....."then500
499 an=an+1
500 nextam
501 printzh$"                                     "
502 printzh$zh$zh$"                                     "
503 ifan-1<3thenprintzh$"nicht zulaessig ! mannschaftsanzahl < 3"
504 ifan-1<3thenan=0:gosub709:return
505 an=0
506 printzh$"     welche mannschaft 1 -";a;:input " ";mw
507 printzh$"  "za$" ich korrigiere die daten von "mw;ze$"   "zb$
508 gosub571
509 fori=1tob
510 forj=1toc
511 ifd(i,j)<>mwande(i,j)<>mwthen515
512 g$(i,d(i,j))="":h$(i,d(i,j))=""
513 g$(i,e(i,j))="":h$(i,e(i,j))=""
514 d(i,j)=0:e(i,j)=0
515 nextj
516 nexti
517 m$(mw)="..............."
518 return
519 fori=int(b/2)+1tob
520 forj=1toc
521 d(i,j)=e(i-b/2,j)
522 e(i,j)=d(i-b/2,j)
523 nextj
524 nexti
525 return
526 printzh$"                                     "
527 printzh$zh$zh$"                                     "
528 ifa+1>20thenprintzh$"nicht zulaessig ! mannschaftsanzahl >20"
529 ifa+1>20thengosub709:return
530 a=a+1:gosub571
531 printzh$zh$:print"  neue mannschaft   ...............";
532 printze$ze$ze$ze$ze$ze$ze$ze$ze$ze$ze$ze$ze$ze$ze$ze$ze$;:inputm$(a)
533 ifint(a/2)=a/2thenreturn
534 fori=b-2toa+1step-1
535 ds$(i)=ds$(i-2)
536 nexti
537 ds$(a)="......198."
538 ds$(a-1)="......198."
539 ds$(b-1)="......198."
540 ds$(b)="......198."
541 :print:print
542 fori=1to150
543 printza$" spieltage und ergebnisse neu setzen "zb$
544 ifi=150then550
545 printzh$;
546 print"                                     "zh$
547 print"                                     "zh$
548 print"                                     "zh$
549 nexti
550 gosub709
551 return
552 printzc$:forx=1to7:printzg$:nextx
553 ifa>15thenprintzh$zh$
554 ifa>16thenprintzh$
555 ifa>17thenprintzh$zh$
556 ifa>18thenprintzg$zh$
557 ifa>19thenprintzh$zh$
558 forj=1toc
559 gg$="":hh$="":n$(i,j)="":g$(i,d(i,j))="":h$(i,d(i,j))=""
560 g$(i,e(i,j))="":h$(i,e(i,j))="":forx=1to17:printzf$;:nextx
561 inputgg$:ifleft$(gg$,3)="..."thengg$="..."
562 ifleft$(gg$,1)="n"thenn$(i,j)=left$(gg$,1):gg$="..."
563 ifgg$<>"..."theng$(i,d(i,j))=str$(val(left$(gg$,3)))
564 printzh$;:forx=1to25:printzf$;:nextx
565 inputhh$:ifleft$(hh$,3)<>"..."thenh$(i,d(i,j))=str$(val(left$(hh$,3))):hh$=""
566 ifn$(i,j)="n"thenh$(i,d(i,j))="":ifn$(i,j)="n"theng$(i,d(i,j))=""
567 g$(i,e(i,j))=h$(i,d(i,j))
568 h$(i,e(i,j))=g$(i,d(i,j))
569 nextj
570 return
571 ifint(a/2)=a/2thenb=(a*2)-2:c=a/2
572 ifint(a/2)<>a/2thenb=a*2:c=(a-1)/2
573 return
574 print:print"           ";za$" formatieren "zb$:print
575 fori=1to20
576 ifm$(i)=""thenm$(i)="..............."
577 nexti
578 fori=1to38
579 ifds$(i)=""thends$(i)="......198."
580 nexti
581 fori=1to38
582 forj=1to10
583 ifdn$(i,j)=""thendn$(i,j)="......198."
584 nextj
585 nexti
586 return
587 print:print"           ";za$" formatieren "zb$:print
588 fori=1tob
589 forj=1toc
590 d(i,j)=d(i,j)+10
591 e(i,j)=e(i,j)+10
592 ifn$(i,j)=""thenn$(i,j)="h"
593 nextj
594 nexti
595 fori=1tob
596 forj=1toa
597 ifg$(i,j)=""theng$(i,j)="800"
598 ifh$(i,j)=""thenh$(i,j)="800"
599 g$(i,j)=str$(val(g$(i,j))+100)
600 h$(i,j)=str$(val(h$(i,j))+100)
601 nextj
602 nexti
603 return
604 print:print"           ";za$"reformatieren"zb$:print
605 gosub571
606 fori=1tob
607 forj=1toc
608 d(i,j)=d(i,j)-10
609 e(i,j)=e(i,j)-10
610 ifn$(i,j)="h"thenn$(i,j)=""
611 nextj
612 nexti
613 fori=1tob
614 forj=1toa
615 g$(i,j)=str$(val(g$(i,j))-100)
616 h$(i,j)=str$(val(h$(i,j))-100)
617 ifg$(i,j)=" 800"theng$(i,j)=""
618 ifh$(i,j)=" 800"thenh$(i,j)=""
619 nextj
620 nexti
621 return
622 fori=1toa
623 sm$=str$(1000+l(i,3))
624 ifl(i,3)<0thensm$=chr$(48)+sm$
625 ifl(i,3)>=0thensm$=chr$(49)+right$(sm$,3)
626 z(i)=i
627 sr$(i)=str$(l(i,8)+100)+str$(999-l(i,9))+sm$+str$(100+l(i,1))+str$(l(i,2))
628 nexti
629 n1=1:n2=a
630 i=1
631 j1=n1:j2=n2
632 ifsr$(j1)>=sr$(j2)then640
633 x=z(j1):z(j1)=z(j2):z(j2)=x
634 x$=sr$(j1):sr$(j1)=sr$(j2):sr$(j2)=x$
635 j1=j1+1
636 ifj1=j2then642
637 ifsr$(j1)>=sr$(j2)then635
638 x=z(j1):z(j1)=z(j2):z(j2)=x
639 x$=sr$(j1):sr$(j1)=sr$(j2):sr$(j2)=x$
640 j2=j2-1
641 ifj2<>j1then632
642 j2=j2+1
643 ifj2>=n2then646
644 k1(i,1)=j2:k1(i,2)=n2
645 i=i+1
646 j1=j1-1
647 ifn1>=j1then650
648 n2=j1
649 goto631
650 i=i-1
651 ifi<1then654
652 n1=k1(i,1):n2=k1(i,2)
653 goto631
654 return
655 za$=chr$(18):zb$=chr$(146):zc$=chr$(19):zd$=chr$(147)
656 ze$=chr$(157):zf$=chr$(29):zg$=chr$(17):zh$=chr$(145)
657 zz$="b{$a0}i{$a0}l{$a0}d{$a0}s{$a0}c{$a0}h{$a0}i{$a0}r{$a0}m"
658 zy$="d{$a0}r{$a0}u{$a0}c{$a0}k{$a0}e{$a0}r{$a0}"
659 zx$="d{$a0}i{$a0}s{$a0}k{$a0}e{$a0}t{$a0}t{$a0}e"
660 zv$="return"
661 zw$="mannschaften         -e-s-k-"
662 zt$="spieltage            -e-s-k-"
663 zs$="tabellen               -s-  "
664 zr$="mannschaftsliste       -d-  "
665 zq$="spieltage              -d-  "
666 zp$="tabelle                -d-  "
667 zo$="daten laden von diskette    "
668 zn$="daten speichern auf diskette"
669 zm$=" neue liga  /   progammende "
670 zl$="   e=eingeben s=sichten k=korrigieren"
671 zk$=" m=menue k=korrigieren crsr=blaettern  "
672 z1$="ligatab{cbm-l}       m{$a0}e{$a0}n{$a0}u{$a0}e{$a0}    {cbm-j}by heuser"
673 z2$="ligatab{cbm-l}mannschaften -e-s-k- {cbm-j}by heuser"
674 z3$="ligatab{cbm-l}   spieltage -e-s-k- {cbm-j}by heuser"
675 z4$="ligatab{cbm-l}    tabellen   -s-   {cbm-j}by heuser"
676 z5$="ligatab{cbm-l}mannschaftsliste -d- {cbm-j}by heuser"
677 z6$="ligatab{cbm-l}   spieltage   -d-   {cbm-j}by heuser"
678 z7$="ligatab{cbm-l}    tabellen   -d-   {cbm-j}by heuser"
679 z8$="ligatab{cbm-l} daten von disk. la. {cbm-j}by heuser"
680 z9$="ligatab{cbm-l} daten auf disk. sp. {cbm-j}by heuser"
681 y9$="ligatab{cbm-l} neue liga/prg.-ende {cbm-j}by heuser"
682 z0$="ligatab{cbm-l} eingabe ok (j/n)    {cbm-j}by heuser"
683 nz$="ligatab{cbm-l}nachholspiele -e-s-k-{cbm-j}by heuser"
684 return
685 gosub571
686 fori=1tob
687 printzd$za$nz$zb$:print
688 printi;". spieltag vom ";ds$(i):print:nz=0
689 forj=1toc
690 ifn$(i,j)<>"n"then692
691 printm$(d(i,j));" - ";m$(e(i,j)):nz=1:print"^^ neues datum :   ";dn$(i,j)
692 nextj
693 ifnz=0thenprint" keine nachholspiele !!!"
694 print:printza$zk$zb$
695 gosub709
696 ifw$=zh$theni=i:ifi=btheni=0
697 ifw$=zg$theni=i-2:ifi<0theni=b-1
698 ifw$="k"thengosub702
699 ifw$="m"thenreturn
700 nexti
701 return
702 printzc$zg$zg$zg$zg$
703 forj=1toc
704 ifn$(i,j)<>"n"then706
705 input"^^ neues datum : ";dn$(i,j):printzg$zh$
706 nextj
707 return
708 gosub723
709 gosub723:gosub730
710 return
711 print#2,"ligatab";chr$(16)chr$(54)chr$(57);"by heuser":return
712 printzd$za$z3$zb$:print
713 forx=1toastep2
714 printx;
715 ifx<10thenprint" ";
716 printm$(x);
717 ifx+1<=athenprintx+1;
718 ifx+1<=aandx+1<10thenprint" ";
719 ifx+1<=athenprintm$(x+1)
720 nextx
721 print:ifx+1>aandx+1<20thenprint
722 return
723 si=54272:fl=si:fh=si+1:tl=si+2:th=si+3:wt=si+4:at=si+5:ht=si+6:lt=si+24
724 pokelt,15:poketh,15:poketl,15:pokeat,0*16+0:pokeht,15*16
725 poke wt,65
726 for xt=50to0step-2:pokefh,40:pokefl,xt:next
727 for xt=50to0step-4:pokefh,40:pokefl,xt:next
728 pokewt,0
729 return
730 getw$:ifw$=""then730
731 return
732 ifg$(i,d(i,j))=""theng$(i,d(i,j))=" ..."
733 ifh$(i,d(i,j))=""thenh$(i,d(i,j))=" ..."
734 return
735 ifg$(i,d(i,j))=" ..."theng$(i,d(i,j))=""
736 ifh$(i,d(i,j))=" ..."thenh$(i,d(i,j))=""
737 return
738 poke 56334,peek(56334)and 254:return
739 poke 56334,peek(56334) or 1:return
740 close15:close2:printchr$(147);za$z8$zb$:print
741 open15,8,15,"i0":open2,8,2,"#"
742 t=18:s=1
743 print#15,"u1";2;0;t;s
744 print#15,"b-p";2;0
745 get#2,xx$:ifxx$=""thenxx$=chr$(0)
746 t=asc(xx$)
747 get#2,xx$:ifxx$=""thenxx$=chr$(0)
748 s=asc(xx$)
749 forxy=0to7
750 print#15,"b-p";2;xy*32+2
751 get#2,xx$:ifxx$=""thenxx$=chr$(0)
752 ifasc(xx$)=0then762
753 print#15,"b-p";2;xy*32+5
754 ff$=""
755 foryx=0to15
756 get#2,xx$:ifxx$=""thenxx$=chr$(0)
757 ifasc(xx$)=160then760
758 ff$=ff$+xx$
759 nextyx
760 ifaa=0thenaa=1:print"   ";ff$:goto762
761 aa=0:print"   ";ff$
762 nextxy
763 ift<>0then743
764 close2:close15
765 return
