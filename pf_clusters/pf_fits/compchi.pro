PRO compchi

;# options
!QUIET = 1
myhome = GETENV('HOME')
file   = 's_results/all_results.log'
nomode = 'flat'                 ;# the mode NOT to use

;# read data
restore,myhome+'/research/redux/scripts/s_resultstemplate.sav'
data    = read_ascii(file, template = s_resultstemplate)
obsids  = data.obsid
datname = data.cluster
k0      = data.k0
k0lo    = k0-data.k0err
chi     = data.chisq
modes   = data.tmode
IF nomode EQ 'itpl' THEN BEGIN
   ind1 = 0
   ind2 = 1
ENDIF ELSE BEGIN
   ind1 = 2
   ind2 = 3
ENDELSE

;# loop through each cluster
prevname = 'fdjfhdjf'
num = 1
FOR i = 0,n_elements(datname)-1 DO BEGIN
   ord = -100
   name = strcompress(datname[i],/remove_all)
   IF modes[i] EQ nomode THEN GOTO,ERROR 
   ord = where(datname EQ name)
   IF ord[0] EQ -1 THEN GOTO,ERROR
   tchi = chi[ord]
   tk0 = k0[ord]
   tk0lo = k0lo[ord]
   IF (tk0lo[ind1] LE 0.) THEN BEGIN
      IF name NE prevname THEN BEGIN
         print, ''
         print, format='(A-40,A15,A10,A10)','## K0 statistically consistent with zero: ',"Name","K0","K0lo"
         print, format='(A-40,A15,F10.3,F10.3)','## K0 statistically consistent with zero: ',name,tk0[ind1],tk0lo[ind1]
      ENDIF
   ENDIF
   IF (tchi[ind1] GT tchi[ind2]) THEN BEGIN
                                ; $ OR tchi[ind1]/tchi[ind2] GT 0.50) 
      IF name NE prevname THEN BEGIN
         push, ox, tchi[ind1]
         push, oy, tchi[ind2]
         push, onames, name
         push, onums, num
         var = textoidl(',   '+num2str(tchi[ind1],2)+',   '+num2str(tchi[ind2],2))
         push, ochi, var
         num++
         print, ''
         print, format='(A-40,A15,A10,A10)','## K0=0 chisq <= K0!=0: ',"Name","K0=0","K0!=0"
         print, format='(A-40,A15,F10.3,F10.3)','## K0=0 chisq <= K0!=0: ',name,tchi[ind2],tchi[ind1]
      ENDIF
   ENDIF ELSE BEGIN
      push, allchik0, tchi[ind1]
      push, allchizero, tchi[ind2]
   ENDELSE
   prevname = name
ERROR:
   IF ord[0] EQ -1 THEN print, '## ERROR '+name
ENDFOR

set_plot, 'PS'
device, $
  filename = 'chicomp.eps', $
  /encapsulated, $
  /portrait, $
  /helvetica
!FANCY    = 4
!LINETYPE = 0
!P.FONT   = 0
!X.THICK  = 3
!Y.THICK  = 3
!Z.THICK  = 3

xmin = 0.75*min(allchik0)
xmax = 1.25*max(allchik0)
ymin = 0.75*min(allchizero)
ymax = 1.25*max(allchizero)
xtex = textoidl('\chi_{red}^2 [K_0\neq0]')
ytex = textoidl('\chi_{red}^2 [K_0=0]')
plotsym, 0, 0.8, /fill
plot, allchik0, allchizero, $
      linestyle = 0, $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax],$
      xtitle = xtex, $
      ytitle = ytex, $
      /xsty, /ysty, $
      /xlog, /ylog, $
      charsize = 0.8, $
      psym = 8
x = maken(1d-6,1d6,10)
y = x
oplot, x, y, psym=0, linestyle=2
plotsym, 0, 0.8
oplot, ox, oy, psym=8
plotsym, 0, 0.4, /fill
oplot, ox, oy, psym=8
items = [textoidl('Num -- Name -- K_0 \neq 0 -- K_0 = 0'), $
         num2str(onums)+': '+onames+ochi]
linearr = replicate(-99,n_elements(items))
psyarr = replicate(-99,n_elements(items))
legend, items, linestyle=linearr, psym=psyarr, charsize=0.5, $
        /top, box=0, /left_legend
xyouts, ox, oy, onums, charsize=0.5, alignment=0.45

device,/close
!QUIET=0

END
