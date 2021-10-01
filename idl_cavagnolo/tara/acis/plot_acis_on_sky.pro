PRO plot_acis_on_sky, S3=s3, MARGIN=margin, $
		      CENTER=center, ARCSEC_PER_CM=arcsec_per_cm

;; The parameter "margin" is the offset (in arcsec) from nominal pointing
;; that is possible due to pointing errors and due to dither.
;; Suggested values are 32" (17" pointing error plus 15" dither),
;; 45" (30" pointing error plus 15" dither), and 
;; 50" (30" pointing error plus 20" dither).


;; The chip corners expressed below are the positions of pixels
;; (2,2), (2,4093), (4093,2), & (4093,4093) projected onto the tangent
;; plane coordinate system (by Jonathan McDowell, Feb 1998), expressed as
;; offsets from the aimpoint in arcsec.

;; Jonathan gave me Focal Plane Coordinates AXAF-FP-1.1.
;; I want to translate this system so that origin is at aimpoint, so I 
;; have to compute:
;; 	xoffset = (FPX - FPX0) * delta_s
;; 	yoffset = (FPY - FPY0) * delta_s
;; where
delta_s = 0.492
fpx0    = 4096.5
fpy0    = 4096.5

;; The right_edge & left_edge tags hold the positions of the S-array CCD
;; edges as positive offsets from the S3 aimpoint (obtained from SOP-01
;; figure) in mm.

st = { min_x:0.0, max_x:0.0, min_y:0.0, max_y:0.0, $
       right_edge:0.0, left_edge:0.0 }
ccd = replicate( st, 10 )

if (n_elements(margin) EQ 0) then margin = 45.0

if keyword_set(s3) then begin
  aimname = 'S3'
  
  ccd.min_x = delta_s * $
  	([4182, 3157, 4182, 3157, 6416, 5391, 4366, 3341, 2317, 1292] - fpx0)
  ccd.max_x = delta_s * $
  	([5186, 4160, 5186, 4160, 7421, 6396, 5371, 4346, 3321, 2296] - fpx0)
  ccd.min_y = delta_s * $
  	([5960, 5960, 4932, 4932, 3595, 3595, 3595, 3595, 3595, 3595] - fpy0)
  ccd.max_y = delta_s * $
  	([6964, 6964, 5935, 5935, 4599, 4599, 4599, 4599, 4599, 4599] - fpy0)
  
  ccd.left_edge = [0,0,0,0, 56.48, 31.47,  6.46, -18.54, -43.55, -68.56]
  ccd.right_edge= [0,0,0,0, 81.06, 56.05, 31.04,   6.04, -18.97, -43.99]
  
  if (NOT keyword_set(center)) then center = [100,500]
endif else begin
  aimname = 'I3'
  ccd.min_x = delta_s * $
  	([4178, 3153, 4178, 3153, 6412, 5387, 4362, 3337, 2312, 1287] - fpx0)
  ccd.max_x = delta_s * $
  	([5182, 4156, 5182, 4156, 7417, 6392, 5367, 4342, 3317, 2292] - fpx0)
  ccd.min_y = delta_s * $
  	([4181, 4181, 3153, 3153, 1816, 1816, 1816, 1816, 1816, 1816] - fpy0)
  ccd.max_y = delta_s * $
  	([5185, 5185, 4156, 4156, 2820, 2820, 2820, 2820, 2820, 2820] - fpy0)
  
  if (NOT keyword_set(center)) then center = [100,-300]
endelse

;; Figure out the axis ranges that will produce a 1-1 aspect ratio plot
;; with the specified center and scale.
set_plot,'ps
device, /LANDSCAPE,xsize=10,ysize=7.5,xoffset=0.5,yoffset=10.5,/inch

xlen_est = !D.X_SIZE - !D.X_CH_SIZE * total( !X.margin )
ylen_est = !D.Y_SIZE - !D.Y_CH_SIZE * total( !Y.margin )

if (NOT keyword_set(arcsec_per_cm)) then arcsec_per_cm = 150
pixel_size = arcsec_per_cm / !D.X_PX_CM

xrange = center[0] + pixel_size * xlen_est * [-0.5,0.5]
yrange = center[1] + pixel_size * ylen_est * [-0.5,0.5]

subti=string(margin,f='(I0," arcsec inset depicts pointing error & dither motion; hot pixels & columns marked")')

plot, [0,1], /NODATA, XRANGE=xrange, YRANGE=yrange, /XSTYLE, /YSTYLE, $
	XTITLE='DETX Offset from '+aimname+' aimpoint (arcsec)', $
	YTITLE='DETY Offset from '+aimname+' aimpoint (arcsec)', $
TITLE='ACIS Projected on Plane Tangent to Mirror Spherical Coordinates', $
	SUBTI=subti

for ii = 0,9 do begin
  ;; First, draw all the CCD boundaries.
  min_x = ccd[ii].min_x
  max_x = ccd[ii].max_x
  min_y = ccd[ii].min_y
  max_y = ccd[ii].max_y
  plots, NOCLIP=0, [min_x, max_x, max_x, min_x, min_x], $
  		   [min_y, min_y, max_y, max_y, min_y]

  ;; Draw danger zone graphics inset by margin.
  min_x = min_x + margin
  max_x = max_x - margin
  min_y = min_y + margin
  max_y = max_y - margin
  plots, NOCLIP=0, [min_x, max_x, max_x, min_x, min_x], $
  		   [min_y, min_y, max_y, max_y, min_y], LINE=1
endfor

;; Mark the aimpoint and label BI CCD's.
oplot, [0], [0], PSYM=-4

xyouts, ALIGN=0.5, NOCLIP=0, (ccd[3].min_x+ccd[3].max_x)/2., $
	(ccd[3].min_y+ccd[3].max_y)/2., 'I3'
xyouts, ALIGN=0.5, NOCLIP=0, (ccd[5].min_x+ccd[5].max_x)/2., $
	(ccd[5].min_y+ccd[5].max_y)/2., 'S1 (BI)'
xyouts, ALIGN=0.5, NOCLIP=0, (ccd[7].min_x+ccd[7].max_x)/2., $
	(ccd[7].min_y+ccd[7].max_y)/2., 'S3 (BI)'

  	      
;; Mark the bad pixels and columns.
bad_x   = [303,0, $
	   (1024-303),(1024-409),0,0, $
	   585,351, $
	   497]
	   
bad_y   = [(1024-253),(1024-72), $
	   253,600,303,409, $
	   383,792, $
	   0]
	   
bad_ccd = [0,0, 1,1,1,1, 5,5, 7]

for ii = 0, n_elements(bad_x)-1 do begin
  chip = ccd[ bad_ccd[ii] ]
  x = chip.min_x + (chip.max_x-chip.min_x)*(bad_x[ii]-2)/(1023-2)
  y = chip.min_y + (chip.max_y-chip.min_y)*(bad_y[ii]-2)/(1023-2)
  
  if (bad_x[ii] EQ 0) then begin
    x=[chip.min_x, chip.max_x]
    y=[y, y]
  endif else if (bad_y[ii] EQ 0) then begin
    x=[x, x]
    y=[chip.min_y, chip.max_y]
  endif else begin
    x = [x]
    y = [y]
  endelse
  
  oplot, x, y, PSYM=-7
endfor


;; Show the grating energies at each danger zone edge.
;; The conversion from position to energy was obtained from the MARX
;; program get_hetg_spect.pro (Feb 1998).
if keyword_set(s3) then begin
  lineskip = (convert_coord(!D.x_ch_size, !D.y_ch_size, /DEV, /TO_DATA) - $
  	      convert_coord(0, 0, /DEV, /TO_DATA))[1]
  top    = ccd[9].max_y + 0.5 * lineskip
  bottom = ccd[9].min_y - 1.5 * lineskip
  
  xyouts, ALIGN=0.0, NOCLIP=0, ccd[9].min_x, top+lineskip,    'HETG energies:'
  xyouts, ALIGN=0.0, NOCLIP=0, ccd[9].min_x, bottom-lineskip, 'METG energies:'
  
  for ii = 9, 5, -1 do begin
    ;; Compute the horizontal offsets (mm) from aimpoint for the left and
    ;; right margins around the current gap.
    ;; Margin is in arcsec; 0.495"/pixel; 0.024 mm/pixel
    left_margin  = ccd[ii].right_edge  - (margin / 0.495 * 0.024 )
    right_margin = ccd[ii-1].left_edge + (margin / 0.495 * 0.024 )
    
    ;; Convert from horizontal offsets to distance along the dispersion X,
    ;; assuming a 5 degree leg angle.
    left_margin  = abs( cos( 5 * !PI / 180. ) * left_margin )
    right_margin = abs( cos( 5 * !PI / 180. ) * right_margin )
    
    ;; Compute the corresponding HETG energies and display.
    left_energy  = (12.3985/left_margin) *(8634./2002.)
    right_energy = (12.3985/right_margin)*(8634./2002.)
    
    txt = string(left_energy,right_energy,f='(F5.2,"--",F5.2)')
    gap_x = (ccd[ii].max_x + ccd[ii-1].min_x) / 2.0
    xyouts, ALIGN=0.5, NOCLIP=0, gap_x, top, strcompress( txt, /REM )
    
    
    ;; Compute the corresponding METG energies and display.
    left_energy  = (12.3985/left_margin) *(8634./4001.)
    right_energy = (12.3985/right_margin)*(8634./4001.)
    
    txt = string(left_energy,right_energy,f='(F4.2,"--",F4.2)')
    xyouts, ALIGN=0.5, NOCLIP=0, gap_x, bottom,    txt
  endfor
endif

;; Show orientation of RA & DEC.
arrow, /NORM, THICK=2.0, HTHICK=2.0, 0.2, 0.85, 0.15, 0.85
xyouts, /NORM, ALIGN=1.0, 0.15, 0.85, 'RA'

arrow, /NORM, THICK=2.0, HTHICK=2.0, 0.2, 0.85, 0.2, 0.9
xyouts, /NORM, ALIGN=0.5, 0.2, 0.9, 'DEC'

device, /CLOSE
set_plot,'X
return
end
