PRO TAG_SPEC,STRUCT,INDEX,TAG_STRING,TAG_DESCRIPTOR
;
code = ['','B','I','J','F','D','','A','']
tags = strlowcase(tag_names(struct))
nind = n_elements(index)
tag_string = ''
tag_descriptor = ''
for i=0,nind-1 do begin 
    tag_string = [tag_string,tags(index(i))]
    descrn = strtrim(string(n_elements(struct.(index(i)))),2)
    descrt = code(datatype(struct.(index(i)),2))
    tag_descriptor = tag_descriptor+descrn+descrt
    if i lt nind-1 then tag_descriptor = tag_descriptor+','
endfor
tag_string = tag_string(1:*)

END

;==============================================================
PRO IMCONT,IMA,HDRA,IMB,HDRB,GRID=GRID,_EXTRA=EXTRA,CHATTER=CHATTER,$
    NOERASE=NOERASE
;
astro = 1
rgb = 0
if n_elements(grid) eq 0 then grid = 1
case n_params() of

0: begin
    print,' usage: IMCONT,IM_DISP,HDR_DISP[,IM_CONT,HDR_CONT,/NOERASE]'
    print,'        optional keywords: GRID=GRID,<most graphics keywords>'
    return
   end

1: begin
    astro = 0
    print,' INFO: no image found for contouring - will use image 1'
    imim = ima
    imct = ima
   end

2: begin
    print,' INFO: no image found for contouring - will use image 1 and header 1'
    imim = ima
    imct = ima
    hdrim = hdra
    hdrct = hdra
    check_fits,imim,hdrim,/update   ;force header to be consistent with image
    check_fits,imct,hdrct,/update   ;force header to be consistent with image
   end 

3: begin
    print,' INFO: no header found for 2nd image - will use header 1'
    imim = ima
    imct = imb
    hdrim = hdra
    hdrct = hdra
    check_fits,imim,hdrim,/update   ;force header to be consistent with image
    check_fits,imct,hdrct,/update   ;force header to be consistent with image
   end

4: begin
    one_im = 0
    imim = ima
    imct = imb
    hdrim = hdra
    hdrct = hdrb
    check_fits,imim,hdrim,/update   ;force header to be consistent with image
    check_fits,imct,hdrct,/update   ;force header to be consistent with image
   end

else: begin
    print,' usage: IMCONT,IM_DISP,HDR_DISP[,IM_CONT,HDR_CONT,/NOERASE]'
    print,'        optional keywords: GRID=GRID,<most graphics keywords>'
    return
   end
endcase

hdrdim = size(hdrim)
if hdrdim(0) gt 1 then begin
   print,' ERROR: 2nd parameter must be a FITS header'
   return
endif
hdrdim = size(hdrct)
if hdrdim(0) gt 1 then begin
   print,' ERROR: 4th parameter must be a FITS header'
   return
endif
imdim = size(imim)
if imdim(0) ne 2 then begin
   print,' ERROR: 1st parameter must be a 2d image'
   return
endif
ctdim = size(imct)
if ctdim(0) ne 2 then begin
   print,' ERROR: 3rd parameter must be a 2d image'
   return
endif

xdim = float(imdim(1))
ydim = float(imdim(2))

if n_elements(chatter) gt 0 then chatter = 1 else chatter = 0

if astro then begin
get_astro:
   gsss = sxpar(hdrim,'PPO1',COUNT=N_ppo1)
   if N_ppo1 EQ 1 then gsss_stdast,hdrim
   gsss = sxpar(hdrct,'PPO1',COUNT=N_ppo1)
   if N_ppo1 EQ 1 then gsss_stdast,hdrct

   ctyp = sxpar(hdrim,'ctype*',count=n_ctype)
   if n_ctype gt 0 and strmid(ctyp(0),0,7) eq 'sky_pix' then begin
      sxaddpar,hdrim,'ctype1','RA---TAN'
      sxaddpar,hdrim,'ctype2','DEC--TAN'
      print,' WARNING: header 1 contains bizarre CTYPE specifications - astrometry may be screwed up!'
   endif
   ctyp = sxpar(hdrct,'ctype*',count=n_ctype)
   if n_ctype gt 0 and strmid(ctyp(0),0,7) eq 'sky_pix' then begin
      sxaddpar,hdrct,'ctype1','RA---TAN'
      sxaddpar,hdrct,'ctype2','DEC--TAN'
      print,' WARNING: header 2 contains bizarre CTYPE specifications - astrometry may be screwed up!'
   endif
   equi = sxpar(hdrim,'equinox',count=n_equi)
   if n_equi le 0 then begin
      print,' WARNING: header 1 contains no EQUINOX keyword - astrometry may be screwed up!'
      equinox = '(equinox unknown)'
   endif else equinox = '('+strtrim(string(equi,form='(i4)'),2)+')'
   equi = sxpar(hdrct,'equinox',count=n_equi)
   if n_equi le 0 then begin
      print,' WARNING: header 2 contains no EQUINOX information - astrometry may be screwed up!'
   endif

   extast,hdrim,astrim    ; extract astrometry parameters for displayed image
   extast,hdrct,astrct    ; extract astrometry parameters for contoured image
 
   xyad,hdrim,(indgen(3)*(xdim-1)/2)#replicate(1,3),$
              replicate(1,3)#((indgen(3)*(ydim-1)/2)),ra,dec
   ramin = min(ra,iramin) & ramax = max(ra,iramax)
   decmin = min(dec,idecmin) & decmax = max(dec,idecmax)

;   find P.A. of North: 

   adxy,hdrim,(ramax+ramin)/2*[1,1],(decmax+decmin)/2+[-0.1,0.1],x,y
   if x(0) gt x(1) then begin
      pa = atan((y(1)-y(0))/(x(1)-x(0))) + !pi/2
   endif else if x(0) lt x(1) then begin
      pa = atan((y(1)-y(0))/(x(1)-x(0))) - !pi/2
   endif else begin
      pa = 0 + !pi*(y(0) gt y(1))
   endelse

   pa = 180/!pi*pa

   if abs(pa) gt 20 then begin
      print,' image misoriented (north is not up) - please rotate, i.e., type:'
      print,'              HROT,IM,HDR,-1,-1,'+strtrim(string(pa,form='(f5.1)'),2)+',-1,-1,2'
      print,'                   then run IMCONT again'
      return
   endif

   if (iramin mod 3) lt (iramax mod 3) then ramax = ramax+360.
   ntics = 8
   if grid gt 0 then ntics = fix(ntics*grid)
   case xdim gt ydim of
   1: begin
        nxtics = ntics
        nytics = 3>fix(nxtics*ydim/xdim)
      end
   0: begin
        nytics = ntics
        nxtics = 3>fix(nytics*xdim/ydim)
      end
   endcase
   raticmin = min(ra(*,0)) & raticmax = max(ra(*,0))
   decticmin = min(dec(0,*)) & decticmax = max(dec(0,*))
   xticsize = xdim/nxtics
   tics,raticmin,raticmax,xdim-1,xticsize,ra_incr,/ra
   nxtics = round(xdim/xticsize)+2
   tic_one,raticmin,xticsize,ra_incr,ra_1,ratic_1,/ra
   ticlabels,ra_1,nxtics,ra_incr,xticlab,/ra,delta=1
   yticsize = ydim/nytics
   tics,decticmin,decticmax,ydim-1,yticsize,dec_incr
   nytics = round(ydim/yticsize)+2
   tic_one,decticmin,yticsize,dec_incr,dec_1,dectic_1
   ticlabels,dec_1,nytics,dec_incr,yticlab,delta=1
   if nxtics gt 30 or nytics gt 30 then begin
      print,' ERROR: number of tick marks to large; reset GRID keyword to smaller value'
      return
   endif

   if chatter then print,' INFO: dynamic range before HASTROM: ',minmax(imct)
   hastrom,imct,hdrct,hdrim,missing=0,interp=2
   if chatter then print,'       dynamic range after  HASTROM: ',minmax(imct)

endif

x = findgen(101)*xdim/100 
y = findgen(101)*ydim/100 

imimmax = float(max(imim,min=imimmin))
imctmax = float(max(imct,min=imctmin))

; disentangle optional keywords for PLOT and CONTOUR commands:

if keyword_set(extra) then begin
   if strlowcase(!version.os) eq 'vms' then vms = 1 else vms = 0
   tags = strlowcase(tag_names(extra))
   ptag_str = ''
   ptag_descr = ''
   ctag_str = ''
   ctag_descr = ''
   cind = -1
   pind = -1
   ind = where(strmid(tags,0,3) eq 'lev',ct)   ; 'levels'
   if ct gt 0 then cind = [cind,ind]
   ind = where(strmid(tags,0,2) eq 'nl',ct)    ; 'nlevels'
   if ct gt 0 then cind = [cind,ind]
   ind = where(strmid(tags,0,2) eq 'c_',ct)    ; 'c_linestyle' etc
   if ct gt 0 then cind = [cind,ind]
   if ct gt 0 then tag_spec,extra,ind,ctag_str,ctag_descr
   ind = where(strmid(tags,0,4) eq 'down',ct)  ; 'downhill'
   if ct gt 0 then cind = [cind,ind]
   ind = where(strmid(tags,0,3) eq 'fil',ct)   ; 'fill'
   if ct gt 0 then cind = [cind,ind]
   ind = where(strmid(tags,0,3) eq 'fol',ct)   ; 'follow'
   if ct gt 0 then cind = [cind,ind]
   if n_elements(cind) gt 1 then begin
      cind = cind(1:*)
      tag_spec,extra,cind,ctag_str,ctag_descr
      cname = 'contour_keywords'
      case vms of
	0: filename = getenv('HOME')+'/'+cname+'*.imcont'      
	1: filename = getenv('HOME')+cname+'*.imcont'      
      endcase
      list = findfile(filename, COUNT = Nfile)
      if (Nfile GT 0) then begin
         list = list(0)
	 filebreak,list,nvfile=cname
         cname = strmid(cname,0,17)$
                        +string(fix(strmid(cname,17,3))+1,form='(i3.3)')
         case vms of
            0: spawn,'mv '+list(0)+' '+getenv('HOME')+'/'+cname+'.imcont'
	    1: spawn,'rename/nolog '+list(0)+' '+cname+'.imcont'
	 endcase
      endif else begin
         cname = cname+'_000'
         case vms of
	   0: filename = getenv('HOME')+'/'+cname+'.imcont'      
	   1: filename = getenv('HOME')+cname+'.imcont'      
         endcase
         openw,unit,filename,/get_lun
         close,unit
         free_lun,unit
      endelse
      create_struct,cextra,cname,ctag_str,ctag_descr
      copy_struct,extra,cextra,select_tag=ctag_str
      if n_tags(cextra) lt n_tags(extra) then begin
         pind = indgen(n_tags(extra))
         remove,cind,pind
         tag_spec,extra,pind,ptag_str,ptag_descr
         pname = 'plot_keywords'
         path = break_path(!path)
         case vms of
	   0: filename = getenv('HOME')+'/'+pname+'*.imcont'      
	   1: filename = getenv('HOME')+pname+'*.imcont'      
         endcase
         list = findfile(filename, COUNT = Nfile)
         if (Nfile GT 0) then begin
            pname = strmid(pname,0,14)$
                           +string(fix(strmid(pname,14,3))+1,form='(i3.3)')
            case vms of
               0: spawn,'mv '+list(0)+' '+getenv('HOME')+'/'+pname+'.imcont'
               1: spawn,'rename/nolog '+list(0)+' '+pname+'.imcont'
	    endcase
         endif else begin
            pname = pname+'_000'
            case vms of
	      0: filename = getenv('HOME')+'/'+pname+'.imcont'      
	      1: filename = getenv('HOME')+pname+'.imcont'      
            endcase
            openw,unit,filename,/get_lun
            close,unit
            free_lun,unit
         endelse
         create_struct,pextra,pname,ptag_str,ptag_descr
         copy_struct,extra,pextra,select_tag=ptag_str
      endif
   endif else begin
      pextra = extra
   endelse
endif

psave = !p
xsave = !x
ysave = !y

!x.tickname = replicate(' ',30)
!y.tickname = replicate(' ',30)
!x.ticks = 1
!y.ticks = 1
!x.minor = 1
!y.minor = 1
!x.tickv = minmax(x)
!y.tickv = minmax(y)
!p.ticklen = 0.0001
!x.title = 'X'
!y.title = 'Y'

if astro then begin
   !x.title = 'Right Ascension '+equinox
   !y.title = 'Declination '+equinox
   xtickv = xdim-1 - (ratic_1+indgen(nxtics)*xticsize)
   ix = where(xtickv ge 0 and xtickv le xdim)
   !x.tickv = xtickv(ix)   
   !x.tickname = xticlab(ix)
   !x.ticks = n_elements(ix) - 1
   ytickv = dectic_1+indgen(nytics)*yticsize
   iy = where(ytickv ge 0 and ytickv le ydim)
   !y.tickv = ytickv(iy)
   !y.tickname = yticlab(iy)
   !y.ticks = n_elements(iy) - 1
endif   

; if !d.name eq 'PS' then device,xsize=18,xoff=2,ysize=27.5,yoff=1,bit=8
; the following assumes A4 paper!
if !d.name eq 'PS' then $     
	if xdim/ydim gt 20/29. then device,bit=8,xsize=18,xoff=1,$
                     		       ysize=18.*ydim/xdim,$
				       yoff=14.5-9*ydim/xdim $
	else $
				    device,bit=8,ysize=27,yoff=1,$
				       xsize=27*ydim/xdim,$
				       xoff=10-13.5*ydim/xdim
if !d.name eq 'X' and !d.window eq -1 then $
                  window,xsize=600*sqrt(xdim/ydim),ysi=600*sqrt(ydim/xdim)

nxplot = !p.multi(1)>1
nyplot = !p.multi(2)>1
chscale = 1
if !p.charsize gt 0 then chscale = !p.charsize
if (nxplot>nyplot) gt 2 then chscale = 0.5*chscale

xoff = (yoff = 0.)
xrsize = !d.x_vsize/nxplot
xpsize = xrsize-total(!x.margin)*!d.x_ch_size*chscale 
ypsize = xpsize*ydim/xdim
yrsize = ypsize+total(!y.margin)*!d.y_ch_size*chscale
yoff = 0.5*(!d.y_vsize-nyplot*yrsize)
if yrsize gt !d.y_vsize/nyplot then begin 
   yrsize = !d.y_vsize/nyplot                        
   ypsize = yrsize-total(!y.margin)*!d.y_ch_size*chscale
   xpsize = ypsize*xdim/ydim
   xrsize = xpsize+total(!x.margin)*!d.x_ch_size*chscale
   xoff = 0.5*(!d.x_vsize-nxplot*xrsize)
   yoff = 0.
endif
psize = convert_coord(xpsize,ypsize,/device,/to_normal)
rsize = convert_coord(xrsize,yrsize,/device,/to_normal)
offset = convert_coord(xoff,yoff,/device,/to_normal)
mplot = shift(reverse(indgen(nxplot*nyplot)),1)
iplot = mplot(!p.multi(0))
xr = ((iplot mod nxplot)+[0,1])*rsize(0) + offset(0) 
yr = 1-(iplot/nxplot+[1,0])*rsize(1)     - offset(1)
!p.region = [xr(0),yr(0),xr(1),yr(1)]

if not keyword_set(noerase) then begin 
;print,!x.range,!x.crange,!y.range,!y.crange,minmax(x),minmax(y)
   if n_elements(pextra) gt 0 then plot,x,y,/xst,/yst,/nodata,_extra=pextra $
                           else plot,x,y,/xst,/yst,/nodata
;print,!x.range,!x.crange,!y.range,!y.crange,minmax(x),minmax(y)
   tvlct,r,g,b,/get

   if !d.name eq 'PS' THEN BEGIN
;         r(!d.n_colors-1) = 255          ;background is black
;         g(!d.n_colors-1) = 255          ;reset highest color index to White
;         b(!d.n_colors-1) = 255
         r(0) = 255          ;background is black
         g(0) = 255          ;reset highest color index to White
         b(0) = 255
      tvlct,r,g,b
      tv,!d.n_colors-1-bytscl(imim,top=!d.n_colors-2),!x.window(0),$
                             !y.window(0),xsize=!x.window(1)-!x.window(0),$
                             ysize=!y.window(1)-!y.window(0),/norm
   endif else begin
      if r(!p.background)+g(!p.background)+b(!p.background) eq 0 then begin
         r(!d.n_colors-1) = 255          ;background is black
         g(!d.n_colors-1) = 255          ;reset highest color index to White
         b(!d.n_colors-1) = 255
      endif else begin
         r(!d.n_colors-1) = 0       ;reset highest color index to Black
         g(!d.n_colors-1) = 0
         b(!d.n_colors-1) = 0
      endelse
      tvlct,r,g,b
      px = !x.window * !d.x_vsize
      py = !y.window * !d.y_vsize
      sx = px(1)-px(0)+1
      sy = py(1)-py(0)+1
      tv,congrid(bytscl(imim,top=!d.n_colors-2),sx,sy),px(0),py(0)
   endelse

   if astro and grid gt 0 then begin
      ra = ra_1
      while ra gt ramin do ra = ra - 15*ra_incr/60.
      while ra lt ramax do begin
          ra = ra+15*ra_incr/60.
          yg = findgen(ydim+1)
          xg = cons_ra(ra,yg,astrim)                      
          oplot,xg,yg,col=!d.n_colors/2
      endwhile
      dec = dec_1
      while dec gt decmin do dec = dec - dec_incr/60.
      while dec lt decmax do begin
          dec = dec+dec_incr/60.
          xg = findgen(xdim+1)
          yg = cons_dec(dec,xg,astrim)
          oplot,xg,yg,col=!d.n_colors/2
      endwhile
   endif

endif

;stop

if n_elements(cextra) gt 0 then contour,imct,findgen(xdim)+0.5,$
                                findgen(ydim)+0.5,/overplot,_extra=cextra $
                           else contour,imct,findgen(xdim)+0.5,$
                                findgen(ydim)+0.5,/overplot

if n_elements(pextra) gt 0 then plot,x,y,/xst,/yst,/nodata,/noerase,_extra=pextra $
                           else plot,x,y,/xst,/yst,/nodata,/noerase

!x.tickname = xsave.tickname
!y.tickname = ysave.tickname
!x.ticks = xsave.ticks
!y.ticks = ysave.ticks
!x.minor = xsave.minor
!y.minor = ysave.minor
!x.tickv = xsave.tickv
!y.tickv = ysave.tickv
!p.ticklen = psave.ticklen
!x.title = xsave.title
!y.title = ysave.title

END






