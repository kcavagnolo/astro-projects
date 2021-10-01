pro compdat, type, dat1, dat2, dat3, dat4

;####################################################
;####################################################
;
; types:
; "src": src vs. src
; "tx": tx vs. tx
; "ftxtx": frac tx vs. tx
; "ftxftx": frac tx vs. frac tx
; "chi": chi vs. chi
;
; ie:
;compdat, 'ftxftx', 'simulta_adj_r2500-50_nhfro_fefree_7-7.dat', 'simulta_adj_r2500-50_nhfro_fefree_2-7.dat','../../me_temp_proj/me_fits/dat/culled_r2500-50_7-7.dat', '../../me_temp_proj/me_fits/dat/culled_r2500-50_2-7.dat'
;
;compdat, 'tx', '../scripts/adj_r2500-50_nhfro_fefree_7-7.dat','../scripts/adj_r2500-50_nhfree_fefree_7-7.dat'
;
;compdat, 'tx', '../scripts/simulta_adj_r2500-50_nhfro_fefree_7-7.dat','../scripts/simulta_adj_r2500-50_nhfree_fefree_7-7.dat'
;
;compdat, 'ftxftx', '../scripts/adj_r2500-50_nhfro_fefree_7-7.dat','../scripts/adj_r2500-50_nhfro_fefree_2-7.dat', $
;                   '../scripts/local_r2500-50_nhfro_fefree_7-7.dat','../scripts/local_r2500-50_nhfro_fefree_2-7.dat'
;
;####################################################
;####################################################

; restore the fit template and read some variables
restore,"/Users/cavagnolo/research/redux/scripts/xspectemp_rin_normerr_src.sav"
file1 = read_ascii(dat1, template = xspectemp_rin_normerr_src)
file2 = read_ascii(dat2, template = xspectemp_rin_normerr_src)

; retrieve only those obsids which are common to both files
obsids = file1.obsid
names = file1.cluster
FOR jj=0,n_elements(obsids)-1 DO BEGIN
    tempobs = obsids[jj]
    a = where(file2.obsid EQ tempobs)
    a = a[0]
    IF a NE -1 THEN BEGIN
        push, get1, jj
        push, get2, a 
    ENDIF ELSE print, names[jj],' ',tempobs,' not in ',dat2
ENDFOR
obsids = obsids[get1]
names = names[get1]

; get values for plotting
CASE type OF
    'src': BEGIN
        x = file1.src
        x = x[get1]
        y = file2.src
        y = y[get2]
        errplot = "no"
        plotnames = "no"
        plotlegend = "no"
        xtx = ''+dat1+textoidl(' SRC %')
        ytx = ''+dat2+textoidl(' SRC %')
        xrange = [0,100]
        yrange = [0,100]
        xover = indgen(100)
        yover = xover
        output = 'comp_src.ps'
    END

    'chi': BEGIN
        x = file1.chisq
        x = x[get1]
        y = file2.chisq
        y = y[get2]
        errplot = "no"
        plotnames = "yes"
        plotlegend = "yes"
        cthick = 0.8
        csize = 0.5
        space = 0.6
        FOR i=0,n_elements(y)-1 DO BEGIN
            IF ((y[i] GE 1.4) OR $
                (y[i] LE 0.8) OR $
                (x[i] GE 1.4) OR $
                (x[i] LE 0.8) OR $
                (x[i]/y[i] GE 1.1) OR $
                (x[i]/y[i] LE 0.9)) THEN BEGIN
                push,plname,strcompress(i,/remove_all)
                push,num,strcompress(i,/remove_all)+'= '+strcompress(names[i],/remove_all)
            ENDIF ELSE BEGIN
                push,plname," "
                push,num," "
            ENDELSE
        ENDFOR
        ord = where(num NE " ")
        IF ord[0] LT 0 THEN num = 'none' ELSE num = num[where(num NE " ")]
        items = ['Labeled',num2str(num)]
        linearr = replicate(-99,n_elements(items))
        psyarr = replicate(-99,n_elements(items))
        xtx = ''+dat1+textoidl(' T_{X} [keV]')
        ytx = ''+dat2+textoidl(' T_{X} [keV]')
        IF min(x) GT min(y) THEN rmin=min(x) ELSE rmin=min(y)
        IF max(x) GT max(y) THEN rmax=max(x) ELSE rmax=max(y)
        xrange = [rmin-0.25*rmin,rmax+0.1*rmax]
        yrange = [rmin-0.25*rmin,rmax+0.1*rmax]
        xover = indgen(100)
        yover = xover
        output = 'comp_chi.ps'
    END

    'tx': BEGIN
        file1tx  = file1.tx
        file1tlo = file1.tlo
        file1thi = file1.thi
        file1tx  = file1tx[get1]
        file1tlo = file1tlo[get1]
        file1thi = file1thi[get1]
        file2tx  = file2.tx
        file2tlo = file2.tlo
        file2thi = file2.thi
        file2tx  = file2tx[get2]
        file2tlo = file2tlo[get2]
        file2thi = file2thi[get2]
        x = file1tx
        y = file2tx
        errplot = "yes"
        plotnames = "yes"
        plotlegend = "yes"
        cthick = 0.8
        csize = 0.5
        space = 0.6
        FOR i=0,n_elements(y)-1 DO BEGIN
            IF ((x[i]/y[i] GE 1.1) OR $
                (x[i]/y[i] LE 0.9)) THEN BEGIN
                push,plname,strcompress(i,/remove_all)
                push,num,strcompress(i,/remove_all)+'= '+strcompress(names[i],/remove_all)
            ENDIF ELSE BEGIN
                push,plname," "
                push,num," "
            ENDELSE
        ENDFOR
        ord = where(num NE " ")
        IF ord[0] LT 0 THEN num = 'none' ELSE num = num[where(num NE " ")]
        items = ['Labeled',num2str(num)]
        linearr = replicate(-99,n_elements(items))
        psyarr = replicate(-99,n_elements(items))
        xlo = file1tx - file1tlo
        xhi = file1thi - file1tx
        ylo = file2tx - file2tlo
        yhi = file2thi - file2tx
        xtx = ''+dat1+textoidl(' T_{X} [keV]')
        ytx = ''+dat2+textoidl(' T_{X} [keV]')
        IF min(x) GT min(y) THEN rmin=min(x) ELSE rmin=min(y)
        IF max(x) GT max(y) THEN rmax=max(x) ELSE rmax=max(y)
;xrange = [0,20]
;yrange = [0,20]
        xrange = [rmin-0.25*rmin,rmax+0.1*rmax]
        yrange = [rmin-0.25*rmin,rmax+0.1*rmax]
        xover = indgen(100)
        yover = xover
        output = 'comp_tx.ps'
    END

    'ftxtx': BEGIN
        plotnames = "yes"
        plotlegend = "yes"
        errplot = "no"
        file1tx  = file1.tx
        file1tlo = file1.tlo
        file1thi = file1.thi
        file1tx  = file1tx[get1]
        file1tlo = file1tlo[get1]
        file1thi = file1thi[get1]
        file2tx  = file2.tx
        file2tlo = file2.tlo
        file2thi = file2.thi
        file2tx  = file2tx[get2]
        file2tlo = file2tlo[get2]
        file2thi = file2thi[get2]
        x = file1tx
        y = file2tx/file1tx
        t1lof = ((file1tx-file1tlo)/file1tx)^2
        t2lof = ((file2tx-file2tlo)/file2tx)^2
        xlo = x-x
        ylo = y*sqrt(t1lof+t2lof)
        t1hif = ((file1thi-file1tx)/file1tx)^2
        t2hif = ((file2thi-file2tx)/file2tx)^2
        xhi = x-x
        yhi = y*sqrt(t2hif+t1hif)
        FOR i=0,n_elements(y)-1 DO BEGIN
            IF (y[i]-ylo[i] GE 1.1) THEN BEGIN
                push,plname,names[i]
                push,errname,names[i]
            ENDIF ELSE push,plname," "
        ENDFOR
        cthick = 0.8
        csize = 0.5
        space = 0.8
        items = [textoidl('T_{HFR} > 1.1:'),errname]
        linearr = replicate(-99,n_elements(items))
        psyarr = replicate(-99,n_elements(items))
        xrange = [min(x)-0.1*min(x),max(x)+0.05*max(x)]
        yrange = [min(y)-0.1*min(y),max(y)+0.05*max(y)]
        ytx = textoidl('T_{HFR}')
        xtx = textoidl('T_{0.7-7.0} [keV]')
        xover = indgen(100)
        yover = replicate(1,100)
        output = 'comp_ftxtx.ps'
    END

    'ftxftx': BEGIN
        file1tx  = file1.tx
        file1tlo = file1.tlo
        file1thi = file1.thi
        file1tx  = file1tx[get1]
        file1tlo = file1tlo[get1]
        file1thi = file1thi[get1]
        file2tx  = file2.tx
        file2tlo = file2.tlo
        file2thi = file2.thi
        file2tx  = file2tx[get2]
        file2tlo = file2tlo[get2]
        file2thi = file2thi[get2]
        file3 = read_ascii(dat3, template = xspectemp_rin_normerr_src)
        file4 = read_ascii(dat4, template = xspectemp_rin_normerr_src)
        void,get
        FOR jj=0,n_elements(obsids)-1 DO BEGIN
            tempobs = obsids[jj]
            a = where(file3.obsid EQ tempobs)
            a = a[0]
            IF a NE -1 THEN push, get, a ELSE print, names[jj],' ',tempobs,' not in ',dat3
        ENDFOR
        file3tx  = file3.tx
        file3tlo = file3.tlo
        file3thi = file3.thi
        file3tx  = file3tx[get]
        file3tlo = file3tlo[get]
        file3thi = file3thi[get]
        void,get
        FOR jj=0,n_elements(obsids)-1 DO BEGIN
            tempobs = obsids[jj]
            a = where(file3.obsid EQ tempobs)
            a = a[0]
            IF a NE -1 THEN push, get, a ELSE print, names[jj],' ',tempobs,' not in ',dat4
        ENDFOR
        file4tx  = file4.tx
        file4tlo = file4.tlo
        file4thi = file4.thi
        file4tx  = file4tx[get]
        file4tlo = file4tlo[get]
        file4thi = file4thi[get]
        output = 'comp_ftxftx.ps'
        errplot = "no"
        plotnames = "yes"
        plotlegend = "yes"
        cthick = 0.8
        csize = 0.5
        space = 0.8
        x = file2tx/file1tx
        y = file4tx/file3tx
        t1lof = ((file1tx-file1tlo)/file1tx)^2
        t2lof = ((file2tx-file2tlo)/file2tx)^2
        t3lof = ((file3tx-file3tlo)/file3tx)^2
        t4lof = ((file4tx-file4tlo)/file4tx)^2
        xlo = x*sqrt(t2lof+t1lof)
        ylo = y*sqrt(t4lof+t3lof)
        t1hif = ((file1thi-file1tx)/file1tx)^2
        t2hif = ((file2thi-file2tx)/file2tx)^2
        t3hif = ((file3thi-file3tx)/file3tx)^2
        t4hif = ((file4thi-file4tx)/file4tx)^2
        xhi = x*sqrt(t2hif+t1hif)
        yhi = y*sqrt(t4hif+t3hif)
        y = y/x
        ylo = y*sqrt((ylo/y)^2.+(xlo/x)^2.)
        yhi = y*sqrt((yhi/y)^2.+(xhi/x)^2.)
        x = indgen(n_elements(x))
        xlo = x-x
        xhi = xlo
        FOR i=0,n_elements(y)-1 DO BEGIN
            IF ((y[i]-ylo[i] GT 1.0) OR (y[i]+yhi[i] LT 1.0)) THEN BEGIN
                push,plname,names[i]
                push,errname,names[i]
            ENDIF ELSE push,plname," "
        ENDFOR
        IF n_elements(yname) EQ 0 THEN yname='none'
        IF n_elements(xname) EQ 0 THEN xname='none'
        items = [textoidl('Sig change in T_{HFR}:'),errname]
        linearr = replicate(-99,n_elements(items))
        psyarr = replicate(-99,n_elements(items))
        xrange = [-0.001,n_elements(x)+1]
        yrange = [min(y)-0.1*min(y),max(y)+0.05*max(y)]
        xtx = textoidl('Cluster')
        ytx = textoidl('T_{HFR,1}/T_{HFR,2}')
        xover = indgen(1000)
        yover = replicate(1.0,1000)
    END

ENDCASE

; make a hardcopy
set_plot, 'PS'
device, filename=output
!fancy = 4
!p.font = 0
!linetype = 0
!p.font = 0
plotsym, 0, 0.5, /fill
plot, x, y, $
      psym = 8, $
      symsize = 0.8, $
      xtitle = xtx, $
      ytitle = ytx, $
      /xsty, /ysty, $
      xrange = xrange, $
      yrange = yrange, $
      charsize = 1.0

; overplot the hi and lo errorbars in both directions
IF (errplot EQ "yes") THEN BEGIN
    oploterror, x, y, xhi, yhi, psym=8, /hibar
    oploterror, x, y, xlo, ylo, psym=8, /lobar
ENDIF

; overplot names
IF (plotnames EQ "yes") THEN BEGIN
    xyouts, x, y, plname, charthick=cthick, charsize=csize, align=1.0
ENDIF

; overplot legend
IF (plotlegend EQ "yes") THEN BEGIN
    legend, items, linestyle=linearr, psym=psyarr, charsize=csize, spacing=space, /bottom, box=0, /right_legend
ENDIF

; overplot the line x=y
oplot, xover, yover, linestyle=2, psym=0

device, /close

END
