pro calc_regions_image,imgfile,r1,r2,area,totcnts,expfile=expfile,      $
                       outfile=outfile,xcen=xcen,ycen=ycen,xybin=xybin, $
                       mincnts=mincnts
;-----------------------------------------------------------------------
; Name: CALC_REGIONS_IMAGE
;
; Purpose: Produces a regions file for a set of annuli with either
;          fixed width or containing a certain number of counts.
;          
; Inputs: 
;         
;         
; Comments: By default, annuli are extracted out to the edge of the image.
;           
; Revision history:
;       written by Michael Wise		11-18-99
;
;-----------------------------------------------------------------------
;
; Check for correct number of parameters
;
np=n_params(0)
if ((np lt 1) or (np gt 6)) then begin
   print,string(7B),'CALLING SEQUENCE: ', $
   'calc_regions_image,imgfile,r1,r2,sb,area,totcnts,expfile=expfile,', $
   '              outfile=outfile,xcen=xcen,ycen=ycen,xybin=xybin,',$
   '              mincnts=mincnts'
   return   
endif


;
; Handle defaults
;
if (n_elements(xybin) eq 0) then xybin=1


;
; Set default minimum number of counts
;
if (keyword_set(mincnts)) then begin
   if (mincnts eq 1) then mincnts=10000
endif


;
; Read in the image
;
im=readfits(imgfile,hd)


;
; Get pixel scale parameters out of the header
;
CRVAL1P=sxpar(hd,'CRVAL1P','Parameter CRVAL1P not found')
CRPIX1P=sxpar(hd,'CRPIX1P','Parameter CRPIX1P not found')
CDELT1P=sxpar(hd,'CDELT1P','Parameter CDELT1P not found')
CRVAL2P=sxpar(hd,'CRVAL2P','Parameter CRVAL2P not found')
CRPIX2P=sxpar(hd,'CRPIX2P','Parameter CRPIX2P not found')
CDELT2P=sxpar(hd,'CDELT2P','Parameter CDELT2P not found')


;
; A little checking
;
s=size(im)
xsize=s(1)
ysize=s(2)
if (s(0) ne 2) then begin
   print,string(7B),'ERROR: Image must be 2 dimensional'
   return
endif


;
; If provided, read in the exposure map
;
if (n_elements(expfile) ne 0) then begin
   em=readfits(expfile)
;
;  Check to make sure two images are commiserate
;
   se=size(em)
   if (total(s(0:2)-se(0:2)) ne 0.0) then begin
      print,string(7B),'ERROR: Exposure map must match input image'
      return   
   endif
;
;  Convert into a bit map
;
   em(where(em ne 0.0))=1.0
endif else begin
   em=fltarr(xsize,ysize)
   em(*)=1.0
   expfile='NONE'
endelse


;
; Define center if necessary
;
if (n_elements(xcen) eq 0) then xcen=xsize/2
if (n_elements(ycen) eq 0) then ycen=xsize/2


;
; Calculate the Sky X and Y of the image center
;
xc=CRVAL1P+CRPIX1P+CDELT1P*xcen
yc=CRVAL2P+CRPIX2P+CDELT2P*ycen


;
; Create distance mask
;
mask=fltarr(xsize,ysize)

xdist=findgen(xsize)
xdist=abs(xdist-float(xcen))
ydist=findgen(ysize)
ydist=abs(ydist-float(ycen))

for i=0,xsize-1 do begin
    for j=0,ysize-1 do begin
        mask(i,j)=sqrt(xdist(i)*xdist(i)+ydist(j)*ydist(j))
    endfor
endfor


;
; Multiply by exposure mask
;
mask=em*mask
mask(xcen,ycen)=0.01


;
; Determine number of radial bins and define some arrays
;
nrad=fix(max(mask)/xybin)
r1=fltarr(nrad)
r2=fltarr(nrad)
totcnts=fltarr(nrad) 
area=fltarr(nrad)

;
; Loop over bins and calculate the area
;
for i=0,nrad-1 do begin
    r1(i)=i*xybin
    r2(i)=(i+1)*xybin
    if (i gt 0) then j=where( (mask gt r1(i)) and (mask le r2(i)) ) $
                else j=where( (mask gt 0.0) and (mask le r2(i)) )
    area(i)=n_elements(j)
    totcnts(i)=total(float(im(j)))
endfor


;
; If the MINCNTS option was set, add up individual radial bins
; until the minimum counts per annulus is reached.
;
if (keyword_set(mincnts)) then begin
   cumcnts=fltarr(nrad)
   cumarea=fltarr(nrad)
   for i=0,nrad-1 do begin  
       cumcnts(i)=total(totcnts(0:i))
       cumarea(i)=total(area(0:i))
   endfor
   ratio=fix(float(cumcnts)/mincnts)
   j=uniq(ratio)   
   
   nradb=n_elements(j)
   r1b=fltarr(nradb)
   r2b=fltarr(nradb)
   totcntsb=fltarr(nradb) 
   areab=fltarr(nradb)

   r1b(0)=r1(0)
   r2b(0)=r2(j(0))
   totcntsb(0)=total(totcnts(0:j(0)))
   areab(0)=total(area(0:j(0)))
   for i=1,nradb-1 do begin
       k=indgen(j(i)-j(i-1))+j(i-1)+1
       r1b(i)=r2(j(i-1))
       r2b(i)=r2(j(i))
       totcntsb(i)=total(totcnts(k))
       areab(i)=total(area(k))
   endfor

   nrad=nradb
   r1=r1b
   r2=r2b
   totcnts=totcntsb
   area=areab
endif 


;
; Write the output file
;
if (n_elements(outfile) ne 0) then begin
   space='         '
   fmt='$(1x,i6,3x,a9,4(3x,f9.3),2(3x,e13.5))'
   fmt2='$(1x,a6,3x,a9,4(3x,a9),2(3x,a13))'
   get_lun,unit
   openw,unit,outfile
   printf,unit,' '
   printf,unit,'Extracted annular regions file'
   printf,unit,' '
   printf,unit,'Date: ',!stime
   printf,unit,'Input file: ',imgfile
   printf,unit,'Exp. file : ',expfile
   printf,unit,'Sky X     : ',xc
   printf,unit,'Sky Y     : ',yc
   printf,unit,'X Centroid: ',xcen
   printf,unit,'Y Centroid: ',ycen
   printf,unit,'Binsize   : ',xybin
   if (keyword_set(mincnts)) then begin
      printf,unit,'Min counts: ',mincnts
   endif
   printf,unit,' '                        
   printf,unit,' '                        
   printf,unit,fmt2,'Index ','  Type   ','   X Center  ', $
                    '   Y Center  ','   Radius 1  ','   Radius 2  ', $
                    '    Area     ','    Counts   '
   for i=0,nrad-1 do begin
       printf,unit,fmt,i,'annulus',xc,yc,r1(i),r2(i),area(i),totcnts(i)
   endfor
   close,unit
   free_lun,unit
endif


;
; Return to IDL
;
return
end
