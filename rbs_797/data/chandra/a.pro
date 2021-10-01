pro a

;# read blue table
tab_read, 'norm.tab', tcb, table, header
bsma = tab_val(tcb, table, 1)+0.5
bint = tab_val(tcb, table, 2)
binterr = tab_val(tcb, table, 3)
ord = where(binterr GT 1d3, num)
IF num GT 0 THEN BEGIN 
   binterr[ord] = 0.0
   binterr[ord] = max(binterr)
   bint[ord] = bint[ord+1]
   bint[ord] = max(bint)
ENDIF
bell = tab_val(tcb, table, 6)
bellerr = tab_val(tcb, table, 7)
ord = where((bellerr GT 1d3), num)
IF num GT 0 THEN BEGIN 
   bellerr[ord] = 0.0
   bellerr[ord] = max(bellerr)
   bell[ord] = bell[ord+1]
   bell[ord] = max(bell)
ENDIF
bpa = tab_val(tcb, table, 8)
bpaerr = tab_val(tcb, table, 9)
ord = where((bpaerr GT 1d3), num)
IF num GT 0 THEN BEGIN 
   bpaerr[ord] = 0.0
   bpaerr[ord] = max(bpaerr)
   bpa[ord] = bpa[ord+1]
   bpa[ord] = max(bpa)
ENDIF

stop

;# Input parameters
!FANCY    = 4
!LINETYPE = 0
!P.FONT   = 0
!X.THICK  = 2
!Y.THICK  = 2
!Z.THICK  = 2
plotsym, 0, psize, /fill
set_plot, 'PS'
device, filename='surbri.eps', $
        /encapsulated, $
        /portrait, $
        set_font='Times-Roman'
ytex = textoidl('\mu [arbitrary units]')
xtex = textoidl('R [kpc]')
xmin = 0.8*min(bsma)
xmax = 1.2*max(bsma)
ymin = 0.8*min(bint-binterr)
ymax = 1.2*max(bint+binterr)
plot, bsma, bint, $
      /nodata, $
      /xlog, /ylog, $
      /xsty, /ysty, $
      ytitle = ytex, $
      xtitle = xtex, $
      xrange = [xmin,xmax], $
      yrange = [ymin,ymax], $
      charsize = csize
oploterror, bsma, bint, binterr, psym=8
oplot, bsma, bint, psym=8
device, /close
END
