pro linreg

datdir = "dat/"
tempdir = "../scripts/"
psnum = '4'
tmin = 0.
tmax = 20.
dtmin = -1.
dtmax = 1.

; input files array
files = strarr(16)
pstitle = strarr(8)

pstitle[0] = 'r500-50'
pstitle[1] = 'r500-100'
pstitle[2] = 'r2500'
pstitle[3] = 'r2500-50'
pstitle[4] = 'r5000'
pstitle[5] = 'r5000-50'
pstitle[6] = 'robs-70'
pstitle[7] = 'rmax-70'

files[0] = datdir+'adj_r500-50_nhfro_fefro_7-7.dat'
files[1] = datdir+'adj_r500-50_nhfro_fefro_2-7.dat'
files[2] = datdir+'adj_r500-100_nhfro_fefro_7-7.dat'
files[3] = datdir+'adj_r500-100_nhfro_fefro_2-7.dat'
files[4] = datdir+'adj_r2500_nhfro_fefro_7-7.dat'
files[5] = datdir+'adj_r2500_nhfro_fefro_2-7.dat'
files[6] = datdir+'adj_r2500-50_nhfro_fefro_7-7.dat'
files[7] = datdir+'adj_r2500-50_nhfro_fefro_2-7.dat'
files[8] = datdir+'adj_r5000_nhfro_fefro_7-7.dat'
files[9] = datdir+'adj_r5000_nhfro_fefro_2-7.dat'
files[10] = datdir+'adj_r5000-50_nhfro_fefro_7-7.dat'
files[11] = datdir+'adj_r5000-50_nhfro_fefro_2-7.dat'
files[12] = datdir+'adj_robs-70_nhfro_fefro_7-7.dat'
files[13] = datdir+'adj_robs-70_nhfro_fefro_2-7.dat'
files[14] = datdir+'adj_rmax-70_nhfro_fefro_7-7.dat'
files[15] = datdir+'adj_rmax-70_nhfro_fefro_2-7.dat'

i = 0
FOR j = 0,n_elements(pstitle)-1 DO BEGIN

; define inputs
    dat1 = files[i]
    dat2 = files[i+1]

; check for file existance
    check = findfile(dat1,count=count)
    check2 = findfile(dat2,count=count2)
    IF ((count EQ 1) AND (count2 EQ 1)) THEN BEGIN

; define output file name
        output = 'temp_'+strcompress(j,/remove_all)+'.ps'

; restore the fit template and read some variables
        restore,"../scripts/xspectemp_rin_normerr_src.sav"
        A = read_ascii(dat1, template = xspectemp_rin_normerr_src)
        B = read_ascii(dat2, template = xspectemp_rin_normerr_src)

; store the hi and lo errors in variables
        x = A.tx
        y = B.tx
        IF (i NE 0) THEN BEGIN
            void = temporary(xarr)
            void = temporary(yarr)
        ENDIF

        FOR k=0,n_elements(x)-1 DO BEGIN
            IF ((x[k] LT 30.) AND (y[k] LT 30)) THEN BEGIN
                IF (n_elements(allxarr) EQ 0) THEN BEGIN
                    allxarr = x[k]
                    allyarr = y[k]
                ENDIF ELSE BEGIN
                    allxarr = [allxarr,x[k]]
                    allyarr = [allyarr,y[k]]
                ENDELSE
                IF (n_elements(xarr) EQ 0) THEN BEGIN
                    xarr = x[k]
                    yarr = y[k]
                ENDIF ELSE BEGIN
                    xarr = [xarr,x[k]]
                    yarr = [yarr,y[k]]
                ENDELSE
            ENDIF
        ENDFOR

        deltx = (yarr-xarr)/yarr
        result = linfit(yarr,deltx,chisq=chisq,sigma=sigma)
        
        x2 = findgen(100)
        y2 = result[1]*x2+result[0]

; make a hardcopy of plaw fits
        tit = textoidl(''+pstitle[j]+'')
        xtx = textoidl('T_{2.0-7.0}keV')
        ytx = textoidl('\deltaT_{X} = T_{2.0}-T_{0.7}/T_{0.7}')
        set_plot, 'PS'
        device, filename = output, /color
        !fancy = 4
        !linetype = 0
        !p.font = 0
        plot, yarr, deltx, $
              psym = 4, $
              xrange = [tmin,tmax], $
              yrange = [dtmin,dtmax], $
              xstyle = 1, $
              ystyle = 1, $
              title = tit, $
              xtitle = xtx, $
              ytitle = ytx, $
              symsize = 0.5, $
              charsize = 0.75
        red = REFORM([255,0,0],1,3)
        TVLCT, red, 100
        oplot, x2, y2, psym=0, linestyle=0, color=100

; draw the legend
        m = strcompress(sigfig(result[1],2),/remove_all)
        b = strcompress(sigfig(result[0],2),/remove_all)
        mpm = textoidl(m+'\pm'+strcompress(sigfig(sigma[1],1),/remove_all))
        bpm = textoidl(b+'\pm'+strcompress(sigfig(sigma[0],1),/remove_all))
        equa = textoidl('Best Fit: \deltaT_{X} = ('+mpm+')\cdotT_{2.0-7.0} + ('+bpm+')')
        chi = strcompress(sigfig(chisq,3),/remove_all)
        csq  = textoidl('\chi^{2}: '+chi)
        items = [equa, csq]
        linearr = [0, -99]
        psyarr = [0, -99]
        legend, items, linestyle=linearr, psym=psyarr, charsize=0.75, /top, /left_legend, box=0
        
        device, /close

; increment the counter
        i = i+2
    ENDIF ELSE BEGIN
        IF count NE 1 THEN print, "No ",dat1
        IF count2 NE 1 THEN print, "No ",dat2
        i=i+2
    ENDELSE

ENDFOR

; make a hardcopy for all data points in every fit plaw fits
deltx = (allyarr-allxarr)/allyarr
result = linfit(allyarr,deltx,chisq=chisq,sigma=sigma)
        
x2 = findgen(100)
y2 = result[1]*x2+result[0]

tit = textoidl('Combined Data')
xtx = textoidl('T_{2.0-7.0}keV')
ytx = textoidl('\deltaT_{X} = T_{2.0}-T_{0.7}/T_{0.7}')
set_plot, 'PS'
device, filename = 'temp_'+strcompress(j+1,/remove_all)+'.ps', /color
!fancy = 4
!linetype = 0
!p.font = 0
plot, allyarr, deltx, $
      psym = 4, $
      xrange = [tmin,tmax], $
      yrange = [dtmin,dtmax], $
      xstyle = 1, $
      ystyle = 1, $
      title = tit, $
      xtitle = xtx, $
      ytitle = ytx, $
      symsize = 0.5, $
      charsize = 0.75
red = REFORM([255,0,0],1,3)
TVLCT, red, 100
oplot, x2, y2, psym=0, linestyle=0, color=100

; draw the legend
m = strcompress(sigfig(result[1],2),/remove_all)
b = strcompress(sigfig(result[0],2),/remove_all)
mpm = textoidl(m+'\pm'+strcompress(sigfig(sigma[1],1),/remove_all))
bpm = textoidl(b+'\pm'+strcompress(sigfig(sigma[0],1),/remove_all))
equa = textoidl('Best Fit: \deltaT_{X} = ('+mpm+')\cdotT_{2.0-7.0} + ('+bpm+')')
chi = strcompress(sigfig(chisq,3),/remove_all)
csq  = textoidl('\chi^{2}: '+chi)
items = [equa, csq]
linearr = [0, -99]
psyarr = [0, -99]
legend, items, linestyle=linearr, psym=psyarr, charsize=0.75, /top, /left_legend, box=0

device, /close

; make all these ps files into one ps file
SPAWN, 'ls temp_*.ps > list'
SPAWN, 'cat list | perl pscat '+psnum+' linreg.ps'
SPAWN, 'rm -f temp*.ps'
SPAWN, 'rm -f list'

END
