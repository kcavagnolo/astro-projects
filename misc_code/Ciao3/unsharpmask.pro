pro unsharpmask,image,lscale,uscale,outfile=outfile
;-----------------------------------------------------------------------
;
; Name: UNSHARPMASK
;
;
; Purpose: Performs unsharp masking on an input image      
;          
;   
; Inputs: image - name of input image
;	  lscale - lower smoothing scale in pixels. If vector, a grid
;		   of images will be produced
;	  uscale - upper smoothing scale in pixels. If vector, a grid
;		   of images will be produced
;
;       
; Optional Inputs: outfile - name of output fits file 
;		  
;                 
; Comments: 
;           
;           
; Revision history:
;       written by DAR, 2006-03-03 
;	
;-----------------------------------------------------------------------
;
; Check for correct number of parameters
;
np=n_params(0)
if (np ne 3) then begin
   print,string(7B),'CALLING SEQUENCE: ', $
   'unsharpmask, image, lscale, uscale [, outfile=outfile]'
   print,'Note: lscale and uscale may be 1-D arrays (e.g., [2,4,6])'
   return   
endif


;
; Read in image
;
im=readfits(image,hd)
s=size(im)
xsize=s[1]
ysize=s[2]
if (s[0] ne 2) then begin
   print,string(7B),'ERROR: Image must be 2 dimensional'
   return
endif


;
; If lscale is vector,
; itterate over range of values
;
sizel=size(lscale)
sizeu=size(uscale)
if ( (sizel[0] eq 1) and (sizel[1] gt 5) ) then begin
   print,string(7B),'ERROR: lscale must have no more than 5 elements.'
   return
endif
if ( (sizeu[0] eq 1) and (sizeu[1] gt 5) ) then begin
   print,string(7B),'ERROR: uscale must have no more than 5 elements.'
   return
endif
if ( (sizel[0] eq 1) or (sizeu[0] eq 1) ) then begin
   imA={i1:fltarr(xsize,ysize), i2:fltarr(xsize,ysize), i3:fltarr(xsize,ysize), i4:fltarr(xsize,ysize), i5:fltarr(xsize,ysize)}
   imB={i1:fltarr(xsize,ysize), i2:fltarr(xsize,ysize), i3:fltarr(xsize,ysize), i4:fltarr(xsize,ysize), i5:fltarr(xsize,ysize)}
   imC={i1:fltarr(xsize,ysize), i2:fltarr(xsize,ysize), i3:fltarr(xsize,ysize), i4:fltarr(xsize,ysize), i5:fltarr(xsize,ysize)}
   tstring='Unsharp Mask ('
   for i=0,n_elements(lscale)-1 do tstring=tstring+strtrim(string(fix(lscale[i])),2)+' '
   tstring=tstring+':'
   for i=0,n_elements(uscale)-1 do tstring=tstring+' '+strtrim(string(fix(uscale[i])),2)
   tstring=tstring+')'
   window,title=tstring,xsize=xsize*n_elements(uscale),ysize=ysize*n_elements(lscale),retain=2
   pass=0
   for i=0,n_elements(lscale)-1 do begin
      imA.(i)=filter_image(im,fwhm=lscale[i],/all,/NO_FT_CONVOL)
      for j=0,n_elements(uscale)-1 do begin
         if (uscale[j] gt lscale[i]) then begin
            if not pass then imB.(j)=filter_image(im,fwhm=uscale[j],/all,/NO_FT_CONVOL)
            imC.(j)=imA.(i)-imB.(j)
            tvscl,imC.(j),n_elements(uscale)*i+j
         endif
      endfor
      pass=1
   endfor
   if (n_elements(outfile) ne 0) then begin
      imfinal=tvrd()
      writefits,outfile,imfinal,hd
   endif
endif else begin
   window,title='Unsharp Mask ('+strtrim(string(lscale),2)+','+strtrim(string(uscale),2)+')',$
          xsize=xsize,ysize=ysize
   imA=filter_image(im,fwhm=lscale,/all,/NO_FT_CONVOL)
   imB=filter_image(im,fwhm=uscale,/all,/NO_FT_CONVOL)
   imC=(imA-imB)
   tvscl,imC,0
   if (n_elements(outfile) ne 0) then writefits,outfile,imC,hd
endelse


;
; Return to IDL
;
return
end