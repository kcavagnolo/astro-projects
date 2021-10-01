pro calc_hetg_weights,order=order,outfile=outfile,hrmafile=hrmafile,$ 
                      hetgfile=hetgfile,caldb=caldb,combine=combine
;-----------------------------------------------------------------------
; Name: CALC_HETG_WEIGHTS
;
; Purpose: Creates a vectors of weights for a specified HETG order
;          appropriate for use with MKINSTMAP to create HETG 
;          exposure maps.
;          
; Inputs:     order -- grating order to consider
;           outfile -- name of ASCII file to contain output
;         
;         
; Comments: 
;           
;           
; Revision history:
;       written by Michael Wise, 08-23-00
;
;-----------------------------------------------------------------------
;
; Check for correct number of parameters
;
np=n_params(0)
if ((np lt 0) or (np gt 20)) then begin
   print,string(7B),'CALLING SEQUENCE: ', $
   'calc_hetg_weights,order=order,outfile=outfile,hrmafile=hrmafile,',  $
   '                  hetgfile=hetgfile,caldb=caldb,combine=combine'
   return   
endif

;
; Set defaults
;
if (n_elements(order) eq 0) then order=0
if (n_elements(outfile) eq 0) then outfile='hetg_weights.dat'

if (n_elements(caldb) eq 0) then $
   caldb='/nfs/apocrypha/d2/wise/data/caldata'

if (n_elements(hrmafile) eq 0) then $
   hrmafile='hrmaD1999-07-22axeffaN0004.fits'

if (n_elements(hetgfile) eq 0) then $
   hetgfile='hetgD1996-11-01greffpr001N0003.fits' 


;
; Read in the HRMA effective areas 
;
;
;unix%dmlist hrmaD1999-07-22axeffaN0004.fits blocks
;----------------------------------------------------------------------------
;Dataset: hrmaD1999-07-22axeffaN0004.fits
;----------------------------------------------------------------------------
;     Block Name                          Type         Dimensions
;----------------------------------------------------------------------------
;Block 1: PRIMARY                        Null        
;Block 2: AXAF_AXEFFA1                   Table         4 cols x 1        rows
;Block 3: AXAF_AXEFFA2                   Table         4 cols x 1        rows
;Block 4: AXAF_AXEFFA3                   Table         4 cols x 1        rows
;Block 5: AXAF_AXEFFA4                   Table         4 cols x 1        rows
;Block 6: AXAF_AXEFFA5                   Table         4 cols x 1        rows
;
;
curfile=caldb+'/'+hrmafile
mt=mrdfits(curfile,1,/silent)	;	Total effective area
m1=mrdfits(curfile,2,/silent)	;	Shell 1 effective area
m3=mrdfits(curfile,3,/silent)	;	Shell 3 effective area
m4=mrdfits(curfile,4,/silent)	;	Shell 4 effective area
m6=mrdfits(curfile,5,/silent)	;	Shell 5 effective area


;
; Read in the HETG grating efficiencies
;
;
;unix% dmlist hetgD1996-11-01greffpr001N0003.fits blocks
;---------------------------------------------------------------------------
;Dataset: hetgD1996-11-01greffpr001N0003.fits
;---------------------------------------------------------------------------
; 
;     Block Name                          Type         Dimensions
;---------------------------------------------------------------------------
;Block    1: PRIMARY                        Null        
;Block    2: AXAF_GREFF1                    Table         3 cols x 1439  rows
;Block    3: AXAF_GREFF2                    Table         3 cols x 1439  rows
;Block    4: AXAF_GREFF3                    Table         3 cols x 1439  rows
;Block    5: AXAF_GREFF4                    Table         3 cols x 1439  rows
;
curfile=caldb+'/'+hetgfile
g1=mrdfits(curfile,1,/silent)	;	Shell 1
g3=mrdfits(curfile,2,/silent)	;	Shell 3
g4=mrdfits(curfile,3,/silent)	;	Shell 4
g6=mrdfits(curfile,4,/silent)	;	Shell 6


;
; Locate the relevant order(s)
;
;    File is assumed to contain +/- orders and 0th orders, i.e.
; 
;    i     =   0  1  2  3  4  5  6
;    order =  -3 -2 -1  0  1  2  3
;
;
norders=n_elements(g1(0).eff)/2     ; Number of orders in array + 0th
if (keyword_set(combine) and (order ne 0)) then begin
   i=[-abs(order),abs(order)]+norders
endif else begin
   i=order+norders
endelse

;
; Make sure order(s) is(are) acceptable
;
lo=where(i lt 0)
hi=where(i gt n_elements(g1(0).eff)-1)
if ( (lo(0) ne -1) or (hi(0) ne -1) ) then begin
   print,string(7B),'ERROR: Value of ORDER is out of range'
   return
endif


;
; Create the grating arrays to be interpolated
;
;	We will use the grating energy array as the relevant
;	baseline array since it many sharp features (not that it 
;	matters given the typical energy resolution use in making 
;	instrument maps).
;
;	We are also assuming the energy grids are the same for
;	all HRMA and HETG extensions, i.e. there are only two
;	different energy grids to consider
;
ex=g1.energy
nume=n_elements(ex)
x1=fltarr(nume)
x3=fltarr(nume)
x4=fltarr(nume)
x6=fltarr(nume)
for j=0,nume-1 do begin
    x1(j)=total(g1(j).eff(i))
    x3(j)=total(g3(j).eff(i))
    x4(j)=total(g4(j).eff(i))
    x6(j)=total(g6(j).eff(i))
endfor


;
; Create the HRMA effective area arrays to be interpolated
;
ey=0.5*(mt.energ_lo+mt.energ_hi)
yt=mt.effarea
y1=m1.effarea/yt
y3=m3.effarea/yt
y4=m4.effarea/yt
y6=m6.effarea/yt


;
; Diagnostics
;
;!p.multi=[0,2,0,0]
;
;plot,ex,x1
;oplot,ex,x3,line=1
;oplot,ex,x4,line=2
;oplot,ex,x6,line=3
;
;plot,ey,y1,yrange=[0.,1.0]
;oplot,ey,y3,line=1
;oplot,ey,y4,line=2
;oplot,ey,y6,line=3
;
;!p.multi(*)=0
;

;
; Interpolate the HRMA effective areas to the HETG grid
;
y1i=spline(ey,y1,ex)
y3i=spline(ey,y3,ex)
y4i=spline(ey,y4,ex)
y6i=spline(ey,y6,ex)


;
; Calculate the weights
;
weights=(x1*y1i+x3*y3i+x4*y4i+x6*y6i)


;
; Write out the results
;
fmt='$(1x,2(3x,e13.5))'
fmt2='$(1H#,2(3x,a13))'

get_lun,unit
openw,unit,outfile
printf,unit,'# '
printf,unit,'# HETG instrument map weights'
printf,unit,'# '
printf,unit,'# Date       : ',!stime
printf,unit,'# '
if (keyword_set(combine) and (order ne 0)) then begin
   printf,unit,'# Order      : ',[-abs(order),abs(order)]
endif else begin
   printf,unit,'# Order      : ',order
endelse
printf,unit,'# HRMA file  : ',hrmafile
printf,unit,'# HETG file  : ',hetgfile
printf,unit,'# '
printf,unit,fmt2,'   Energy','   Weight'
printf,unit,'# '
for j=0,nume-1 do begin
    printf,unit,fmt,ex(j),weights(j)
endfor
close,unit
free_lun,unit


;
; Return to IDL
;
return
end
