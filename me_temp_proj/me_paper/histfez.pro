pro histfez

ymin = 0.01
ymax = 35.
xmin = 0.01
xmax = 1.19

datdir = "../fits/dat/"
csize = 0.7
psize = 0.7
space = 1.0

; input files array
files = strarr(6)
pstitle = strarr(6)

pstitle[0] = textoidl('r_{max-70}')
pstitle[1] = textoidl('r_{break-70}')
pstitle[2] = textoidl('r_{2500-70}')
pstitle[3] = textoidl('r_{5000-70}')
pstitle[4] = textoidl('r_{2500}')
pstitle[5] = textoidl('r_{5000}')

files[0] = datdir+'adj_rmax-70_nhfro_fefree_7-7.dat'
files[1] = datdir+'adj_robs-70_nhfro_fefree_7-7.dat'
files[2] = datdir+'adj_r2500-50_nhfro_fefree_7-7.dat'
files[3] = datdir+'adj_r5000-50_nhfro_fefree_7-7.dat'
files[4] = datdir+'adj_r2500_nhfro_fefree_7-7.dat'
files[5] = datdir+'adj_r5000_nhfro_fefree_7-7.dat'

i = 0
nn = 0
FOR j = 0,n_elements(pstitle)-1 DO BEGIN

    ; define inputs
    in1 = files[i]

    ; check for file existance
    check = findfile(in1,count=count)
    IF (count EQ 1) THEN BEGIN

        ; restore the fit templates
        restore,"../scripts/xspectemp_rin_normerr_src.sav"
        dat1 = read_ascii(in1, template = xspectemp_rin_normerr_src)

        IF i NE 0 THEN BEGIN
            void,lo
            void,hi
            void,sigy
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

; build the empty plot
        odevice = !d.name
        IF nn EQ 0 THEN BEGIN
            set_plot, 'PS'
            device, filename='histfe_z.ps', /color
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
              ytickinterval=10, $
              xrange = [xmin,xmax], $
              yrange = [ymin,ymax], $
              /xsty, $
              /ysty, $
              charsize = csize
        ENDIF ELSE BEGIN
            multiplot & plot, $
              dumx, $
              dumy, $
              /nodata, $
              ytickinterval=10, $
              xrange = [xmin,xmax], $
              yrange = [ymin,ymax], $
              /xsty, $
              /ysty, $
              charsize = csize
        ENDELSE

; begin the power law fitting
        coeffs = [0.1, 0.1]
        weights = 1/(sigy)^2.
        fit = MPCURVEFIT(dat1.z, dat1.fe, weights, coeffs, sigma, function_name='plaw', CHISQ=chi, NFREE=nfree, DOF=dof, /autoderivative, /quiet)

; calculate a wavg
        zwav = total(dat1.fe*weights)/total(weights)
        zswav = 1./sqrt(total(weights))

; define some names and such for the legend
        chi = textoidl('chisq='+num2str(chi/dof,3))
        zw  = textoidl('Z_{wavg}='+num2str(zwav,2)+'\pm'+num2str(zswav,1))
        items = [pstitle[i],zw,chi]
        linearr = replicate(-99,n_elements(items))
        psyarr = replicate(-99,n_elements(items))
        legend, items, linestyle=linearr, psym=psyarr, charsize=csize, spacing=space, /top, box=0, /right_legend

; draw a histogram
        histogram_ez, dat1.fe, $
                      charsize=0.0001, $
                      binsize=0.05, $
                      ytickinterval=10, $
                      /xsty, $
                      /ysty, $
                      xran=[xmin,xmax], $
                      yran=[ymin,ymax]

; overplot the wav on the histogram
        y = findgen(100)
        x = replicate(zwav,n_elements(y))
        oplot, x, y, psym=0, linestyle=1

; increment the counter
        i++
        nn++
    ENDIF ELSE BEGIN
        IF count NE 1 THEN print, "No ",in1
        i++
    ENDELSE
        
ENDFOR

ytex = textoidl('Number of clusters')
xtex = 'Z/Z'+sunsymbol()

; horz label
xyouts, 0.52, 0.1, xtex, /normal, charsize=csize

; vert label
xyouts, 0.12, 0.52, ytex, /normal, orientation=90, charsize=csize

device,/close
set_plot, odevice

END
