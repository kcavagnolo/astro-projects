; make plot of entropy
; as function of gas mass

plottype  = 1 ; 0 = S vs R, 1= S vs. R/Rvir, 2 = S/T vs. R/Rvir, 
              ; 3 = S/T^0.67 vs. R200, 4 = S vs. Mgas
plotextra = "no"
dofit     = "yes"
;make_eps  = "no"
make_eps  = "yes"

H0 = 70
OM = 0.3
L0 = 0.7

trials = [ $
'1tdeproj_nhfree_fefree', '1tdeproj_nhfreetied_fefree', $
'1tdeproj_nhfrozen_fefree', '1tproj_nhfree_fefree', '1tproj_nhfrozen_fefree']

;ii=3

FOR plottype=0,3 DO BEGIN 
FOR ii = 0,4 DO BEGIN 


IF make_eps EQ "yes" THEN ps,trials[ii]+'_sprof'+strtrim(string(plottype),2)+'.eps',/color,/tex,/copy

IF !d.name EQ 'PS' THEN BEGIN 
    csize=1.0
    !p.font=0
    lsize = csize * 0.65
;    lsize = 1.0
    thick = 6
ENDIF ELSE BEGIN 
    csize = 1.0
    lsize = csize
ENDELSE 

; get information from region file
refdata = split(slurp('../scripts/reference.list',comment='#'))
; get fit information
fitdata = split(slurp('../data/'+trials[ii] +'.dat',comment='#'))

; get refdata clusters that are in fitdata
idx = lonarr(n_elements(refdata[0,*]))+1000
FOR i=0,n_elements(refdata[0,*])-1 DO BEGIN 
    idx2 = where(fitdata[0,*] EQ refdata[0,i] AND $
                 fitdata[1,*] EQ refdata[1,i],nmatch)
    IF nmatch NE 0 THEN idx[i] = i
ENDFOR  
idx = idx[where(idx NE 1000)]
refdata = refdata[*,idx]
; sort by tx
refdata = refdata[*,sort(float(refdata[8,*]))]

; define some useful variables
name = refdata[0,*]
ra   = float(refdata[2,*])
dec  = float(refdata[3,*])
rmax = float(refdata[4,*])
tavg = float(refdata[8,*])
z    = float(refdata[6,*])
rvir = rxtx(tavg,200) * (100./H0) *1000

clname = name
FOR i=0,n_elements(name)-1 DO clname[i] = strjoin(strsplit(name[i],'_',/EXTRACT),' ')

CASE plottype OF 
    0: BEGIN 
;        xrange=[0.0006,1.0]
        xrange=[4,300]
        yrange=[1,1000]
        xtitle = textoidl('R [kpc]')
        ytitle = textoidl('S [keV cm^{2}]')
    END 
    1: BEGIN 
;        xrange=[0.0006,1.0]
        xrange=[0.003,0.2]
        yrange=[1,1000]
        xtitle = textoidl('R/R_{200}')
        ytitle = textoidl('S [keV cm^{2}]')
    END 
    2: BEGIN
        xrange=[0.003,0.2]
        yrange=[1,100] 
        xtitle=textoidl('R/R_{200}')
        ytitle=textoidl('S/T_{avg} [cm^{2}]')
    END
    3: BEGIN
        xrange=[0.003,0.2]
        yrange=[1,200] 
        xtitle=textoidl('R/R_{200}')
        ytitle=textoidl('S/T_{avg}^{0.67} [keV^{0.33} cm^{2}]')
    END
    4: BEGIN 
        xrange=[1d9,1d14]
        yrange=[1,1000]
        xtitle=textoidl('Enclosed Gas Mass [M_{sun}]')
        ytitle=textoidl('S [keV cm^{2}]')
    END 
ENDCASE 

; set up empty plot
plot,xrange,yrange,/nodata,xrange=xrange,yrange=yrange,xtitle=xtitle, $
     ytitle=ytitle,/xsty,/ysty,/ylog,/xlog,charsize=csize

; get entropy profile for each cluster
ncl = n_elements(refdata[0,*])     
nplot = ncl
loadct,39
color= (findgen(nplot)+0.5)*(!d.n_colors/nplot)

symbols = [4,5,8,13,14,15,4,5,8,13,14,15,4,5,8,13,14,15,4,5,8,13,14,15, $
           4,5,8,13,14,15,4,5,8,13,14,15,4,5,8,13,14,15,4,5,8,13,14,15]

; define array to hold fit information
p = fltarr(8,ncl)

m=0 & n=ncl
;m=6 & n=m+1
FOR i=m,n-1 DO BEGIN 

    ; get annuli info
    idx2 = where(fitdata[0,*] EQ refdata[0,i] AND fitdata[1,*] EQ refdata[1,i])
    rad  = float(fitdata[2,idx2])*60
    idx3 = idx2[sort(rad)]
    rad  = float(fitdata[2,idx3])*60

    ; get midbin values
    radb = shift(rad,1)
    radb[0] = 0
    rd = (rad-radb)/2
    rmid = radb + rd

    ; deprojected
    nh    = float(fitdata[3,idx3])
    tx    = float(fitdata[6,idx3])
    tlo   = float(fitdata[7,idx3])
    thi   = float(fitdata[8,idx3])
    z     = float(fitdata[17,idx3])
    norm  = float(fitdata[12,idx3])

    ; convert to kpc
    rkpc  = angtolin(rmid,z,H0,OM,LO,/arcsec,/kpc)
    rdkpc = angtolin(rd,z,H0,OM,LO,/arcsec,/kpc)
    rkpc  = rkpc
    rdkpc = rdkpc

    ; derive density and entropy
    rout = angtolin(rad,z,H0,OM,LO,/arcsec,/kpc)*3.085678d+18*1e3
    rin  = angtolin(radb,z,H0,OM,LO,/arcsec,/kpc)*3.085678d+18*1e3
    ;d    = 2.997925e5/(H0*q0^2*(1+z)) * (q0*z + (q0-1)*(sqrt(2*q0*z+1)-1))

    d = lumdist(z, H0 = H0, Lambda0 = L0, Omega_M = OM,/silent)
    dl   = d*(1+z)*3.085678d+18*1e6
    nel  = sqrt( 1.4/1.167 * dl^2 / 1d-14 * 3*norm/(rout^3-rin^3))
    ss   = tx/nel^(2/3.)
    shi  = ss*thi/tx 
    slo  = ss*tlo/tx

    ; derive gas mass
    mgper = 4/3. * !pi * 1.167 * 1.673d-24 * nel * (rout^3-rin^3) / 1.99d33
    mgas  = dblarr(n_elements(rad))
    FOR j=0,n_elements(rad)-1 DO mgas[j] = total(mgper[0:j])

    CASE plottype OF 
        0: BEGIN 
            x   = rkpc
            y   = ss
            ylo = slo
            yhi = shi
        END 
        1: BEGIN 
            x   = rkpc/rvir[i]
            y   = ss
            ylo = slo
            yhi = shi
        END 
        2: BEGIN
            x   = rkpc/rvir[i]
            y   = ss/tavg[i]
            ylo = slo/tavg[i]
            yhi = shi/tavg[i]
        END 
        3: BEGIN
            x   = rkpc/rvir[i]
            y   = ss/(tavg[i]^0.67)
            ylo = slo/(tavg[i]^0.67)
            yhi = shi/(tavg[i]^0.67)
        END 
        4: BEGIN 
            x   = mgas
            y   = ss             
            ylo = slo
            yhi = shi
        END 
    ENDCASE 

    ; plot the entropy
    errorbars,x,y,x,x,ylo,yhi
    oplot,x,y,psym=usesym(symbols[i],/fill),color=color[i]

    ; fit the data
    IF dofit EQ "yes" THEN BEGIN 

        yerr = (yhi-ylo)/2 * 0.61 ; 1sigma

        yidx = where(yerr EQ 0,npt)
        IF npt NE 0 THEN yerr[yidx] = 0.01*y[yidx]

        ; do power law fit
;         params = mpfitexpr('p[0]*x^p[1]',x,y,yerr,[1e3,0.5], $
;                            perror=perror,bestnorm=bestnorm,/quiet)
;         oplot,x,params[0]*x^params[1],line=2,color=color[i]
;         p[0,i] =  params[0]
;         p[1,i] =  perror[0]
;         p[2,i] =  params[1]
;         p[3,i] =  perror[1]
;         p[6,i] =  bestnorm
;         p[7,i] =  n_elements(x)-3

        ; add intercept
        start = [1e3,1.0,10.]
        IF plottype EQ 0 THEN start = [1.,1.0,10.]
        IF plottype EQ 1 THEN start = [1e3,1.0,10.]

        params = mpfitexpr('p[0]*x^p[1]+p[2]',x,y,yerr,start, $
                           perror=perror,bestnorm=bestnorm,/quiet)
        oplot,x,params[0]*x^params[1]+params[2],line=0,color=color[i]
        p[0,i] =  params[0]
        p[1,i] =  perror[0]
        p[2,i] =  params[1]
        p[3,i] =  perror[1]
        p[4,i] =  params[2]
        p[5,i] =  perror[2]
        p[6,i] =  bestnorm
        p[7,i] =  n_elements(x)-3

    ENDIF  

ENDFOR 

;legend,name+textoidl(' (T_{avg} =
;')+strtrim(string(tavg,format='(f10.2)'),2)+'keV, '+textoidl('\alpha
;=
;')
;+strtrim(string(p[2,*],format='(f10.2)'),2)+textoidl('\pm')+strtrim(string(p[3,*],format='(f10.2)'),2)+')',psym=intarr(ncl)+8,usersym=-1*symbols,color=color,charsize=csize,/bottom,/right;,/top,/left
;legend,name+textoidl(' (T_{avg} = ')+strtrim(string(tavg,format='(f10.2)'),2)+'keV, '+textoidl('\alpha = ')+strtrim(string(p[2,*],format='(f10.2)'),2)+')',psym=intarr(ncl)+8,usersym=-1*symbols,color=color,charsize=csize,/bottom,/right
legend,clname+textoidl(' (T_{avg} = ')+strtrim(string(tavg,format='(f10.2)'),2)+'keV, '+textoidl('\alpha = ')+strtrim(string(p[2,*],format='(f10.2)'),2)+', '+textoidl('S_0 = ')+strtrim(string(p[4,*],format='(f10.2)'),2)+')',psym=intarr(ncl)+8,usersym=-1*symbols,color=color,charsize=lsize,/bottom,/right


; plot RM and Arnaud XMM groups

IF plotextra EQ "yes" THEN BEGIN 

    fitdata2 = split(slurp('xmmgroups.dat',comment='#'))
    name2    = fitdata2[0,uniq(fitdata2[0,*],sort(fitdata2[0,*]))]
    tavg2    = fitdata2[10,uniq(fitdata2[0,*],sort(fitdata2[0,*]))]
    rvir2    = rxtx(tavg2,200) / (H0/100) *1000

    n     = n_elements(name2) 
    idx   = sort(tavg2)
    name2 = name2[idx]
    tavg2 = tavg2[idx]
    rvir2 = rvir2[idx]

    ii = i
    ;; symbols = [8,13,14,15,4,5,8,13,14,15,4,5,8,13,14,15]
    symbols =[4,5,8,13,14,15,4,5,8,13,14,15]

    FOR i=0,n-1 DO BEGIN 

        ; get annuli info
        idx2 = where(fitdata2[0,*] EQ name2[i])
        rad  = float(fitdata2[1,idx2])
        idx3 = idx2[sort(rad)]
        rad  = float(fitdata2[1,idx3])

        ; get midbin values
        radb = shift(rad,1)
        radb[0] = 0
        rd = (rad-radb)/2
        rmid = radb + rd

        ; deprojected
        tx2   = float(fitdata2[2,idx3])
        z     = float(fitdata2[8,idx3])
        nel   = float(fitdata2[9,idx3])*sqrt(H0/50)

        ; convert to kpc
        rkpc = angtolin(rmid,z,H0,OM,L0,/arcmin,/kpc)
        rdkpc = angtolin(rd,z,H0,OM,L0,/arcmin,/kpc)
        rkpc = rkpc

        ; derive density and entropy
        rout = angtolin(rad,z,H0,OM,L0,/arcmin,/kpc)*3.085678d+18*1e3
        rin  = angtolin(radb,z,H0,OM,L0,/arcmin,/kpc)*3.085678d+18*1e3
        ss = tx2/nel^(2/3.)

        ; derive gas mass
        mgper = 4/3. * !pi * 1.167 * 1.673d-24 * nel * (rout^3-rin^3) / 1.99d33
        mgas = dblarr(n_elements(rad))
        FOR j=0,n_elements(rad)-1 DO BEGIN 
            mgas[j]    = total(mgper[0:j])
        ENDFOR  

        CASE plottype OF 
            1: BEGIN 
                x = rkpc/rvir2[i]
                y = ss
            END 
            2: BEGIN
                x = rkpc/rvir2[i]
                y = ss/tavg2[i]
            END 
            3: BEGIN 
                x = mgas
                y = ss 
            END 
        ENDCASE 
       oplot,x,y,psym=-1*usesym(symbols[i],/fill),line=0
;       oplot,x,y,psym=-1*usesym(symbols[i],/fill),symsize=1.5,color=color[i+8]
;       oplot,x,y,psym=-1*usesym(symbols[i],/fill),symsize=1.5,color=color[i+8],line=1

    ENDFOR 

    clname2 = strarr(n)
    FOR i=0,n-1 DO clname2[i] = strjoin(strsplit(name2[i],'_',/EXTRACT),' ')
    legend,clname2+textoidl(' (T_{avg} = ')+strtrim(string(tavg2,format='(f10.2)'),2)+'keV) ',/top,/left,psym=intarr(n)+8,usersym=-1*symbols,charsize=lsize

ENDIF  

IF make_eps EQ "yes" THEN psclose

ENDFOR 
ENDFOR 

END 


