pro cc_ncc_bin

myhome = GETENV('HOME')
mpcol = 1
mprow = 2
output = 'cc_ncc_bin.eps'
rfilter = 'no'
zfilter = 'no'
xcfr = 'no'
sig = 2.0
step = 0.05
csize = 0.9
space = 0.7
psize = 0.8
max = 1.6
xmin = 0.77
xmax = 1.62
ymin = -0.1
ymax = 1.1
lower = 1.0
upper = 1.25

;# input files array
push, pstitle, textoidl('R_{2500-CORE}')
push, pstitle, textoidl('R_{5000-CORE}')
push, pstitle, textoidl('R_{2500}')
push, pstitle, textoidl('R_{5000}')

push, regname, 'r2500-core'
push, regname, 'r5000-core'
push, regname, 'r2500'
push, regname, 'r5000'

push, files, myhome+'/research/me_temp_proj/me_fits/dat/culled_r2500-50_7-7.dat'
push, files, myhome+'/research/me_temp_proj/me_fits/dat/culled_r2500-50_2-7.dat'
push, files, myhome+'/research/me_temp_proj/me_fits/dat/culled_r5000-50_7-7.dat'
push, files, myhome+'/research/me_temp_proj/me_fits/dat/culled_r5000-50_2-7.dat'
push, files, myhome+'/research/me_temp_proj/me_fits/dat/culled_r2500_7-7.dat'
push, files, myhome+'/research/me_temp_proj/me_fits/dat/culled_r2500_2-7.dat'
push, files, myhome+'/research/me_temp_proj/me_fits/dat/culled_r5000_7-7.dat'
push, files, myhome+'/research/me_temp_proj/me_fits/dat/culled_r5000_2-7.dat'

corefile = myhome+'/research/me_temp_proj/me_fits/dat/inner50_cash.dat'
;corefile = myhome+'/research/me_temp_proj/me_fits/dat/inner50_chi.dat'
;corefile = myhome+'/research/me_temp_proj/me_fits/dat/inner50_chi2.dat'
relia = myhome+'/research/me_temp_proj/me_fits/fak_ccncc.log'

;push, pstitle, textoidl('R_{2500}')
;push, pstitle, textoidl('R_{5000}')
;push, files, myhome+'/research/me_temp_proj/me_fits/dat/culled_r2500_7-7.dat'
;push, files, myhome+'/research/me_temp_proj/me_fits/dat/culled_r2500_2-7.dat'
;push, files, myhome+'/research/me_temp_proj/me_fits/dat/culled_r5000_7-7.dat'
;push, files, myhome+'/research/me_temp_proj/me_fits/dat/culled_r5000_2-7.dat'

i = 0
m = 0
FOR j = 0,n_elements(pstitle)-1 DO BEGIN
   openw, /get_lun, lun2, 'ccncc_'+strcompress(regname[j])+'.log'
   printf, lun2, format='(A-25, A8, 6A10)',$
           '#Cluster', 'CType', 'THBR', 'hi', 'lo', 'Decr', 'hi', 'lo'
   void, coretx
   void, corelo
   void, corehi
   restore, myhome+'/research/redux/scripts/xspectemp_rin_normerr_src.sav'
   full = read_ascii(files[i], template = xspectemp_rin_normerr_src)
   hard = read_ascii(files[i+1], template = xspectemp_rin_normerr_src)
   core = read_ascii(corefile, template = xspectemp_rin_normerr_src)

    ;# correct for 10% error
    IF xcfr EQ 'yes' THEN BEGIN
        fulltx = full.tx*1.1
        fulllo = fulltx - (full.tlo*1.1)
        fullhi = (full.thi*1.1) - fulltx
        tsig = 1.0
    ENDIF ELSE BEGIN
        fulltx = full.tx
        fulllo = fulltx - full.tlo
        fullhi = full.thi - fulltx
        tsig = 1.1
    ENDELSE
    hardtx = hard.tx
    hardlo = hardtx - hard.tlo
    hardhi = hard.thi - hardtx
    names  = full.cluster
    obsids = full.obsid

    ;# z filter
    IF zfilter EQ 'yes' THEN BEGIN
        z = full.z
        ord    = where(z LE 0.6)
        names  = names[ord]
        fulltx = fulltx[ord]
        fulllo = fulllo[ord]
        fullhi = fullhi[ord]
        hardtx = hardtx[ord]
        hardlo = hardlo[ord]
        hardhi = hardhi[ord]
        obsids = obsids[ord]
    ENDIF

    ;# get core temperatures
    FOR jj=0,n_elements(obsids)-1 DO BEGIN
        tobs = obsids[jj]
        get = where(core.obsid EQ tobs)
        get = get[0]
        IF xcfr EQ 'yes' THEN BEGIN
            ctx = core.tx[get]*1.1
            clo = ctx - (core.tlo[get]*1.1)
            chi = (core.thi[get]*1.1) - ctx
        ENDIF ELSE BEGIN
            ctx = core.tx[get]
            clo = ctx - core.tlo[get]
            chi = core.thi[get] - ctx
        ENDELSE
        push, coretx, ctx
        push, corelo, clo
        push, corehi, chi
    ENDFOR

    ;# do the calcs
    frac   = hardtx/fulltx
    dec    = coretx/fulltx
    frachi = (hardtx/fulltx)*(sqrt((hardhi/hardtx)^2.+(fullhi/fulltx)^2.))
    fraclo = (hardtx/fulltx)*(sqrt((hardlo/hardtx)^2.+(fulllo/fulltx)^2.))
    dechi  = (coretx/fulltx)*(sqrt((corehi/coretx)^2.+(fullhi/fulltx)^2.))
    declo  = (coretx/fulltx)*(sqrt((corelo/coretx)^2.+(fulllo/fulltx)^2.))

    ;# reliab filter
    IF rfilter EQ 'yes' THEN BEGIN
        void, rfactor
        readcol, relia, FORMAT='A,A,A,I,I,F', $
                 rname,robs,rtype,rfcc,rfncc,rper
        FOR jj=0,n_elements(obsids)-1 DO BEGIN
            tobs = obsids[jj]
            get = where(robs EQ tobs)
            get = get[0]
            push, rfactor, rper[get]
        ENDFOR
        get = where(rfactor GE 0.9)
        frac = frac[get]
        frachi = frachi[get]
        fraclo = fraclo[get]
        dec = dec[get]
        dechi = dechi[get]
        declo = declo[get]
        coretx = coretx[get]
        corelo = corelo[get]
        corehi = corehi[get]
        rfactor = rfactor[get]
        names = names[get]
    ENDIF
    
    FOR ss=0,n_elements(frac)-1 DO BEGIN
       IF (frac[ss]-fraclo[ss] GT tsig) THEN BEGIN
          IF (dec[ss]+sig*dechi[ss] LT 1.) THEN ctype = 'CC' ELSE ctype = 'NCC'
          IF n_elements(outname) EQ 0 THEN GOTO,START
          check = where(outname EQ names[ss])
          IF check[0] EQ -1 THEN BEGIN
             START:
             IF j EQ 1 THEN $
                push, outname, names[ss]+' $\ddagger$' ELSE $
                   push, outname, names[ss]
             push, outtf, frac[ss]
             push, outhi, frachi[ss]
             push, outlo, fraclo[ss]
             push, outdec, dec[ss]
             push, outdhi, dechi[ss]
             push, outdlo, declo[ss]
             push, outm, 'Y'
             push, outct, ctype
             push, outobs, obsids[ss]
          ENDIF
       ENDIF
    ENDFOR

    FOR ss=0,n_elements(frac)-1 DO BEGIN
       IF (dec[ss]+sig*dechi[ss] LT 1.) THEN ctype='CC' ELSE ctype='NCC'
       printf, lun2, format='(A-25, A8, 6F10.4)',$
               names[ss], ctype, frac[ss], frachi[ss], fraclo[ss], dec[ss], dechi[ss], declo[ss]
    ENDFOR

    cut = xmin+step
    cc = 0.0
    ncc = 0.0
    mm = 0
    void, numcc
    void, numncc
    void, x
    WHILE cut LE max DO BEGIN
        IF (mm NE 0) THEN BEGIN
            cc = 0
            ncc = 0
        ENDIF
        FOR ss=0,n_elements(frac)-1 DO BEGIN
            IF (dec[ss]+sig*dechi[ss] LT 1. AND frac[ss] GT cut) THEN cc++
            IF (dec[ss]+sig*dechi[ss] GE 1. AND frac[ss] GT cut) THEN ncc++
        ENDFOR
        IF (mm EQ 0) THEN maxcc = cc
        IF (mm EQ 0) THEN maxncc = ncc
        push, numcc, cc
        push, numncc, ncc
        push, x, cut
        cut = cut+step
        mm++
    ENDWHILE
    allcore = numcc + numncc

    IF (m EQ 0) THEN BEGIN
        set_plot,'PS'
        device, filename = output, $
          /encapsulated, $
          /portrait, $
          /helvetica
        !FANCY    = 4
        !LINETYPE = 0
        !P.FONT   = 0
        !X.THICK  = 3
        !Y.THICK  = 3
        !Z.THICK  = 3
        multiplot,[mpcol,mprow]
    ENDIF
    IF (m NE 0) THEN multiplot

    ;# CC clusters
    print, 'Num CC: ',maxcc
    y = numcc/maxcc
    get = where((x GE lower) AND (x LE upper))
    fitx = x[get]
    fity = y[get]
    result = linfit(fitx, fity, chisq=chisq, sigma=sigma, prob=prob, /double)
    print, 'CC Linear fit:'
    print, 'slope = ',num2str(result[1],3),' +/- ',num2str(sigma[1],3)
    print, 'chisq = ',num2str(chisq,3)
    print, 'prob  = ',num2str(prob,3)
    plotsym, 4, /fill
    plot, x, y, $
          /xsty, /ysty, $
          psym = 8, $
          xrange = [xmin,xmax], $
          yrange = [ymin,ymax], $
          symsize = psize, $
          charsize = csize
    oplot, x, y, psym=0, linestyle=1

    ;# NCC clusters
    print, 'Num NCC: ',maxncc
    y = numncc/maxncc
    get = where((x GE lower) AND (x LE upper))
    fitx = x[get]
    fity = y[get]
    result = linfit(fitx, fity, chisq=chisq, sigma=sigma, prob=prob, /double)
    print, 'NCC Linear fit:'
    print, 'slope = ',num2str(result[1],3),' +/- ',num2str(sigma[1],3)
    print, 'chisq = ',num2str(chisq,3)
    print, 'prob  = ',num2str(prob,3)
    plotsym,8, /fill
    oplot, x, y, psym=8, symsize=psize
    oplot, x, y, psym=0, linestyle=2

    ;# draw a legend
    items = [pstitle[j],'CC','NCC']
    linearr = [-99,1,2]
    psyarr = [1,5,6]
    legend, items, linestyle=linearr, psym=psyarr, charsize=csize, $
      /fill, /top, /right_legend, box=0

    i = i+2
    m++
    close, lun2
ENDFOR
xtex = textoidl('T_{HBR,cut}')
ytex = textoidl('Normalized number of clusters with T_{HBR} > T_{HBR,cut}')
xyouts, 0.52, 0.10, xtex, /normal, charsize=csize
xyouts, 0.14, 0.22, ytex, /normal, orientation=90, charsize=csize
device, /close

ord = reverse(sort(outtf-outlo))
name = outname[ord]
fname = outname[ord]
strreplace, fname, '_', ' '
obs = outobs[ord]
tf = outtf[ord]
tfhi = outhi[ord]
tflo = outlo[ord]
dec = outdec[ord]
dhi = outdhi[ord]
dlo = outdlo[ord]
m = outm[ord]
ct = outct[ord]

openw, /get_lun, lun, 'cc_ncc.tex'
printf, lun, '\begin{deluxetable}{lcccclc}'
printf, lun, '\tabletypesize{\scriptsize}'
printf, lun, '\tablecaption{Clusters with \tf$ > 1.1$ at the 1$\sigma$ level.\label{tab:tf12}}'
printf, lun, '\tablewidth{0pt}'
printf, lun, '\tablehead{'
printf, lun, '\colhead{Name} & \colhead{\tf} & \colhead{Merger} & \colhead{Core} & \colhead{Decrement} &'
printf, lun, '\colhead{X-ray Morphology} & \colhead{Ref.}}'
printf, lun, '\startdata'
FOR n=0,n_elements(name)-1 DO BEGIN
    printf, lun, format='(A-30, A-10, F5.2, A4, F4.2, A4, F4.2, A5, A4, A3, A4, A3, F5.2, A4, F4.2, A4, F4.2, A5, A20)', $
            fname[n],$
            '\dotfill & ',$
            tf[n],$
            '$^{+',$
            tfhi[n],$
            '}_{-',$
            tflo[n],$
            '}$ & ',$
            strcompress(m[n],/remove_all),$
            ' & ',$
            strcompress(ct[n],/remove_all),$
            ' & ',$
            dec[n],$
            '$^{+',$
            dhi[n],$
            '}_{-',$
            dlo[n],$
            '}$ & ',$
            'descrip & none\\'
ENDFOR
printf, lun, '\enddata'
printf, lun, '\tablecomments{Clusters ordered by lower limit of \tf.'
printf, lun, '%[R1525] none'
printf, lun, '[1]  \cite{2003A&A...398L...5E}, %[M1008]'
printf, lun, '[2]  \cite{2003ApJ...593..291K}, %[A2034]'
printf, lun, '[3]  \cite{2005ChJAA...5..126Y}, %[A0401]'
printf, lun, '%[R0439] none'
printf, lun, '[4]  \cite{1998ApJ...503...77M}, %[A3376]'
printf, lun, '[5]  \cite{2006Sci...314..791B}, %[A3376]'
printf, lun, '[6]  \cite{1990ApJS...72..715T}, %[A1689]'
printf, lun, '[7]  \cite{2004ApJ...607..190A}, %[A1689]'
printf, lun, '[8]  \cite{1995ApJ...446..583B}, %[A2255]'
printf, lun, '[9]  \cite{1997A&A...317..432F}, %[A2255]'
printf, lun, '[10] \cite{1997ApJ...490...56G}, %[A2218]'
printf, lun, '[11] \cite{2002ApJS..139..313D}, %[A1763]'
printf, lun, '[12] \cite{2005MNRAS.359..417S}, %[A1763]'
printf, lun, '%[M2243] none'
printf, lun, '[13] \cite{1982ApJ...255L..17G}, %[A2069]'
printf, lun, '%[A2384] none'
printf, lun, '[14] \cite{2004ApJ...610L..81H}, %[A0168]'
printf, lun, '[15] \cite{2004ApJ...614..692Y}, %[A0168]'
printf, lun, '[16] \cite{2003A&A...408...57M}, %[A0209]'
printf, lun, '[17] \cite{2000ApJ...540..726G}, %[A0665]'
printf, lun, '[18] \cite{1998ApJ...496L...5T}, %[1E065]'
printf, lun, '%[M0547] none'
printf, lun, '%[Z1215] none'
printf, lun, '%[A1204] none'
printf, lun, '[19] \cite{1999AcA....49..403K}, %[MKW3s]'
printf, lun, '%[M2311] none'
printf, lun, '%[A0267] none'
printf, lun, '[20] \cite{2001ApJ...555..205M}, %[R1720]'
printf, lun, '%[A0907] none'
printf, lun, '[21] \cite{2001A&A...379..807G}, %[A0514]'
printf, lun, '[22] \cite{1998MNRAS.301..609B}, %[A1651]'
printf, lun, '[23] \cite{2005ApJ...619..161G}  %[3C280]'
printf, lun, '%[M1427] none'
printf, lun, '%\cite{2005ApJ...627..733M}, %[A0520]'
printf, lun, '%\cite{1992ApJ...390..345A}, %[A2163]'
printf, lun, '%\cite{1994ApJ...436L..71M}, %[A2163]'
printf, lun, '}'
printf, lun, '\end{deluxetable}'
close, lun
END

