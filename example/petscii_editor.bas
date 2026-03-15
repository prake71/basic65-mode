100 print"{home}{home}{clr}{rvon}yaped-mini{rvof} by ubik {SHIFT--} f1 save {SHIFT--} f2 load  {SHIFT--} f8 insert line"
110 print"important: ensure correct screen mode for saved file{down}{down}"
111 c1=$ff80000
121 sz=2000
125 if rwindow(2)=50 then s1=$40800:sz=4000:else s1=$800
130 if rwindow(2)=40 then sz=1000
140 input "file name? screen{left}{left}{left}{left}{left}{left}{left}{left}";f$:gosub1030
150 key off : e$=chr$(27)
160 print e$"m"e$"r{home}";  : rem disable scrolling & line pushing
170 do
180     cursor on : getkey t$ : cursor off
190     t=asc(t$): if t<133 or t>140 and t<>27 then begin
200     print t$;:if t=34 or t=148 then print e$"o";: rem cancel quote mode
210     bend
220     if t=133 then gosub 1000    : rem f1 = save screen
230     if t=137 then gosub 1030    : rem f2 = load screen
240     if t=140 then print e$"i";  : rem f8 = insert line
250 loop
1000 bsave "@"+f$+".g", p(s1) to p(s1+sz) : rem glyphs
1010 bsave "@"+f$+".c", p(c1) to p(c1+sz) : rem colours
1020 return
1030 trap 1080 : bload (f$+".g"),p(s1) : bload (f$+".c"),p(c1)
1040 trap
1070 return
1080 if er=4 and ds=62 then print"{clr}- new file -":resume 1040
1090 print err$(er):print el:stop
