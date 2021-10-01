pro fez

ymin = 0.01
ymax = 2.
xmin = 0.01
xmax = 2.

errpl = "yes"

datdir = "../me_fits/dat/"
csize = 0.7
space = 0.6
psize = 0.7

; input files array
ctsfile = datdir+'counts_r2500-50.dat'
push, pstitle,textoidl('r_{2500-CORE}')
push, pstitle,textoidl('r_{5000-CORE}')
push, pstitle,textoidl('r_{2500}')
push, pstitle,textoidl('r_{5000}')

push, files,datdir+'culled_r2500-50_7-7.dat'
push, files,datdir+'culled_r5000-50_7-7.dat'
push, files,datdir+'culled_r2500_7-7.dat'
push, files,datdir+'culled_r5000_7-7.dat'

i = 0
nn = 0
FOR j = 0,n_elements(pstitle)-1 DO BEGIN

    ; define inputs
    in1 = files[i]

    ; check for file existance
    check = findfile(in1,count=count)
    IF (count EQ 1) THEN BEGIN

        ; restore the fit templates
        restore,"/Users/cavagnolo/research/redux/scripts/xspectemp_rin_normerr_src.sav"
        dat1 = read_ascii(in1, template = xspectemp_rin_normerr_src)

        restore,datdir+"counts_temp.sav"
        ctdat = read_ascii(ctsfile, template = counts_temp)

        IF i NE 0 THEN BEGIN
            void,lo
            void,hi
            void,sigy
            void,xo20
            void,yo20
            void,yo20lo
            void,yo20hi
            void,xu20
            void,yu20
            void,yu20lo
            void,yu20hi
            void,xfit
            void,yfit
            void,yerr
        ENDIF

        ; store the hi and lo errors in variables
        lo = dat1.fe - dat1.felo
        hi = dat1.fehi - dat1.fe

        ; test to see which deviation is larger, the hi or lo ;
        ; store the larger of the two as the error value
        sigy = fltarr(n_elements(dat1.fe))
        FOR h = 0,n_elements(sigy)-1 DO BEGIN
            IF (lo[h] GT hi[h]) THEN sigy[h] = lo[h] ELSE sigy[h] = hi[h]
        ENDFOR

        FOR ii = 0,n_elements(dat1.obsid)-1 DO BEGIN
            obsid = dat1.obsid[ii]
            FOR aa = 0,n_elements(ctdat.obsid)-1 DO BEGIN
                IF ctdat.obsid[aa] EQ obsid AND ctdat.counts[aa] GE 20000. THEN BEGIN
                    push,xo20,dat1.z[ii]
                    push,yo20,dat1.fe[ii]
                    push,yo20lo,lo[ii]
                    push,yo20hi,hi[ii]
                ENDIF
                IF ctdat.obsid[aa] EQ obsid AND ctdat.counts[aa] LT 20000. THEN BEGIN
                    push,xu20,dat1.z[ii]
                    push,yu20,dat1.fe[ii]
                    push,yu20lo,lo[ii]
                    push,yu20hi,hi[ii]
                ENDIF
            ENDFOR
            IF dat1.fe[ii] LE 1. AND dat1.fe[ii] GE 0.1 THEN BEGIN
                push,xfit,dat1.z[ii]
                push,yfit,dat1.fe[ii]
                push,yerr,sigy[ii]
            ENDIF
        ENDFOR

; build the empty plot
        odevice = !d.name
        IF nn EQ 0 THEN BEGIN
            set_plot, 'PS'
            device, filename='fe_z.ps', /color
            !fancy = 4
            !linetype = 0
            !p.font = 0
            dumx = [0.,2000]
            dumy = dumx
            multiplot,[2,3]
            plot, $
              dumx, $
              dumy, $
              /nodata, $
              /xlog, $
              /ylog, $
              xrange = [xmin,xmax], $
              yrange = [ymin,ymax], $
              /xsty, $
              /ysty, $
              /normal, $
              charsize = csize
        ENDIF ELSE BEGIN
            multiplot & plot, $
              dumx, $
              dumy, $
              /nodata, $
              /xlog, $
              /ylog, $
              xrange = [xmin,xmax], $
              yrange = [ymin,ymax], $
              /xsty, $
              /ysty, $
              /normal, $
              charsize = csize
            ENDELSE

; build color arrays
            red = REFORM([255,0,0],1,3)
            TVLCT, red, 100
            blue = REFORM([0,0,255],1,3)
            TVLCT, blue, 200

; overplot the error
            IF errpl EQ "yes" THEN BEGIN
                oploterror, xo20, yo20, yo20lo, /lobar, errcolor=100, /nohat, symsize = psize, psym=3
                oploterror, xo20, yo20, yo20hi, /hibar, errcolor=100, /nohat, symsize = psize, psym=3
                oploterror, xu20, yu20, yu20lo, /lobar, errcolor=200, /nohat, symsize = psize, psym=3
                oploterror, xu20, yu20, yu20hi, /hibar, errcolor=200, /nohat, symsize = psize, psym=3
            ENDIF

; overplot the points
            plotsym,8,0.5,/fill
            oplot, xo20, yo20, color=100, psym=8, symsize=psize
            oplot, xu20, yu20, color=200, psym=8, symsize=psize
            
; begin the power law fitting
            coeffs = [0.1, 0.1]
            weights = 1/(yerr)^2.
            fit = MPCURVEFIT(xfit, yfit, weights, coeffs, sigma, function_name='plaw', CHISQ=chi, NFREE=nfree, DOF=dof, /autoderivative, /quiet)

; calculate a wavg
            zwav = total(yfit*weights)/total(weights)
            zswav = 1./sqrt(total(weights))

; overplot the wavg
            x = findgen(xmax*5)+xmin
            y = replicate(zwav,n_elements(x))
            oplot, x, y, psym=0, linestyle=1

; define some names and such for the legend
            chi = textoidl('chisq='+num2str(chi/dof,3))
            zw = num2str(zwav,2)
            a = num2str(coeffs[0],2)
            b = num2str(coeffs[1],2)
            zw = textoidl('Z_{wavg}='+zw+'\pm'+num2str(zswav,1))
            a = textoidl('a='+a+'\pm'+num2str(sigma[0],1))
            b = textoidl('b='+b+'\pm'+num2str(sigma[1],1))
            items = [pstitle[i],zw,chi,a,b]
            linearr = replicate(-99,n_elements(items))
            psyarr = replicate(-99,n_elements(items))
            legend, items, linestyle=linearr, psym=psyarr, charsize=csize, spacing=space, /bottom, box=0, /left_legend

; overplot the fit line
            x = findgen(xmax*5)+xmin
            y = coeffs[0]*x^coeffs[1]
            oplot,x,y,psym=0,linestyle=0

; increment the counter
            i++
            nn++
        ENDIF ELSE BEGIN
            IF count NE 1 THEN print, "No ",in1
            i++
        ENDELSE
        
ENDFOR

xtex = textoidl('redshift')
ytex = 'Z/Z'+sunsymbol()

; horz label
xyouts, 0.52, 0.1, xtex, /normal, charsize=csize

; vert label
xyouts, 0.12, 0.52, ytex, /normal, orientation=90, charsize=csize

device,/close
set_plot, odevice

END
