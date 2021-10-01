pro sys_plots

; ie:
; sys_plots

datdir = "dat/"
tempdir = "/Users/cavagnolo/research/redux/scripts/"
prefix = "culled_"
psnum = '4'

tmin = 0.
tmax = 20.
femin = 0.
femax = 2.
chimin = 0.
chimax = 3.
csize = 0.8
plotsym, 0, 0.8, /fill

; input files array
xax = "final"
yax = "local"
files = strarr(8)
pstitle = strarr(4)
pstitle[0] = prefix+'r2500'
pstitle[1] = prefix+'r2500-50'
pstitle[2] = prefix+'r5000'
pstitle[3] = prefix+'r5000-50'
files[0] = 'r25_27.dat'
files[1] = datdir+prefix+'r2500_2-7.dat'
files[2] = 'r25-5_27.dat'
files[3] = datdir+prefix+'r2500-50_2-7.dat'
files[4] = 'r5_27.dat'
files[5] = datdir+prefix+'r5000_2-7.dat'
files[6] = 'r5-5_27.dat'
files[7] = datdir+prefix+'r5000-50_2-7.dat'

;pstitle[0] = prefix+'r2500'
;pstitle[1] = prefix+'r2500-50'
;pstitle[2] = prefix+'r5000'
;pstitle[3] = prefix+'r5000-50'
;files[0] = datdir+prefix+'r2500_nhfro_fefree_7-7.dat'
;files[1] = datdir+prefix+'r2500_nhfree_fefree_7-7.dat'
;files[2] = datdir+prefix+'r2500-50_nhfro_fefree_7-7.dat'
;files[3] = datdir+prefix+'r2500-50_nhfree_fefree_7-7.dat'
;files[4] = datdir+prefix+'r5000_nhfro_fefree_7-7.dat'
;files[5] = datdir+prefix+'r5000_nhfree_fefree_7-7.dat'
;files[6] = datdir+prefix+'r5000-50_nhfro_fefree_7-7.dat'
;files[7] = datdir+prefix+'r5000-50_nhfree_fefree_7-7.dat'

i = 0
FOR j = 0,n_elements(pstitle)-1 DO BEGIN

; define inputs
    file1 = files[i]
    file2 = files[i+1]

; check for file existance
    check = findfile(file1,count=count)
    check2 = findfile(file2,count=count2)
    IF ((count EQ 1) AND (count2 EQ 1)) THEN BEGIN

; define output file name
        output = 'sys_'+pstitle[j]+'.ps'

; restore the fit template and read some variables
        restore,tempdir+'xspectemp_rin_normerr_src.sav'
        dat1 = read_ascii(file1, template = xspectemp_rin_normerr_src)
        dat2 = read_ascii(file2, template = xspectemp_rin_normerr_src)

; define global plotting params
        !fancy = 4
        !p.font = 0
; tx vs. tx
        xtx = textoidl('T_{X} N_{H}-'+xax+' [keV]')
        ytx = textoidl('T_{X} N_{H}-'+yax+' [keV]')
        set_plot, 'PS'
        device, filename = 'temp1.ps'
        plot, dat1.tx, dat2.tx, $
              psym = 8, $
              title = output, $
              xtitle = xtx, $
              ytitle = ytx, $
              xran = [min(dat1.tx)-0.1*(min(dat1.tx)),max(dat1.tx)+0.1*(max(dat1.tx))], $
              yran = [min(dat2.tx)-0.1*(min(dat1.tx)),max(dat2.tx)+0.1*(max(dat2.tx))], $
              /xsty , /ysty, $
              charsize = csize
        oploterror, dat1.tx, dat2.tx, dat1.tx-dat1.tlo, dat2.tx-dat2.tlo, psym=8, /lobar
        oploterror, dat1.tx, dat2.tx, dat1.thi-dat1.tx, dat2.thi-dat2.tx, psym=8, /hibar
        x = findgen(100)
        y = x
        oplot, x, y, psym=0, linestyle=2

; nh vs. nh
        xtx = textoidl('N_{H}-'+xax+' [10^{20} cm^{-2}]')
        ytx = textoidl('N_{H}-'+yax+' [10^{20} cm^{-2}]')
        set_plot, 'PS'
        device, filename = 'temp2.ps'
        plot, dat1.nh, dat2.nh, $
              psym = 8, $
              title = output, $
              xtitle = xtx, $
              ytitle = ytx, $
              xran = [min(dat1.nh)-0.1*(min(dat1.nh)),max(dat1.nh)+0.1*(max(dat1.nh))], $
              yran = [min(dat2.nh)-0.1*(min(dat1.nh)),max(dat2.nh)+0.1*(max(dat2.nh))], $
              /xsty , /ysty, $
              ;/xlog, /ylog, $
              charsize = csize
;        oploterror, dat1.nh, dat2.nh, dat1.nh-dat1.nlo, dat2.nh-dat2.nlo, psym=8, /lobar
;        oploterror, dat1.nh, dat2.nh, dat1.nhi-dat1.nh, dat2.nhi-dat2.nh, psym=8, /hibar
        x = findgen(10000)*0.01
        y = x
        oplot, x, y, psym=0, linestyle=2

; fe vs. fe
        xtx = textoidl('Z/Z_{solar} N_{H}-'+xax+'')
        ytx = textoidl('Z/Z_{solar} N_{H}-'+yax+'')
        set_plot, 'PS'
        device, filename = 'temp3.ps'
        plot, dat1.fe, dat2.fe, $
              psym = 8, $
              title = output, $
              xtitle = xtx, $
              ytitle = ytx, $
              xran = [min(dat1.fe)-0.1*(min(dat1.fe)),max(dat1.fe)+0.1*(max(dat1.fe))], $
              yran = [min(dat2.fe)-0.1*(min(dat1.fe)),max(dat2.fe)+0.1*(max(dat2.fe))], $
              /xsty , /ysty, $
              charsize = csize
        oploterror, dat1.fe, dat2.fe, dat1.fe-dat1.felo, dat2.fe-dat2.felo, psym=8, /lobar
        oploterror, dat1.fe, dat2.fe, dat1.fehi-dat1.fe, dat2.fehi-dat2.fe, psym=8, /hibar
        x = findgen(100)
        y = x
        oplot, x, y, psym=0, linestyle=2

; chi vs. chi
        xtx = textoidl('Red. \chi^{2} N_{H}-'+xax+'')
        ytx = textoidl('Red. \chi^{2} N_{H}-'+yax+'')
        set_plot, 'PS'
        device, filename = 'temp4.ps'
        plot, dat1.chisq, dat2.chisq, $
              psym = 8, $
              title = output, $
              xtitle = xtx, $
              ytitle = ytx, $
              xran = [min(dat1.chisq)-0.1*(min(dat1.chisq)),max(dat1.chisq)+0.1*(max(dat1.chisq))], $
              yran = [min(dat2.chisq)-0.1*(min(dat1.chisq)),max(dat2.chisq)+0.1*(max(dat2.chisq))], $
              /xsty , /ysty, $
              charsize = csize
        x = findgen(100)
        y = x
        oplot, x, y, psym=0, linestyle=2

; close the device
        device, /close

; make all these ps files into one ps file
        SPAWN, 'ls temp*.ps > list'
        SPAWN, 'cat list | perl pscat.pl '+psnum+' '+output
        SPAWN, 'rm -f temp*.ps'
        SPAWN, 'rm -f list'

; increment the counter
        i = i+2
    ENDIF ELSE BEGIN
        IF count NE 1 THEN print, "No ",file1
        IF count2 NE 1 THEN print, "No ",file2
        i=i+2
    ENDELSE
ENDFOR
END
