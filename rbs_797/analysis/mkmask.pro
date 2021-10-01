pro mkmask,image,maskout
;-----------------------------------------------------------------------
;
; Name: MKMASK
;
; Purpose: Creates masks for galaxy photometry
;
; Inputs: image - Name of fits file (STRING)
;         maskout - name of output file (STRING)
;
; Comments: Based on mkmask.pro by BRM
;
;                         **ORIGINAL HEADER**
;*********************************************************************
;This procedure is used to interactively select regions of a displayed
;image to be set to a default pixel value so that the regions will be
;ignored by the photometry routines.
;*********************************************************************
;INPUTS: The displayed image and cursor-selected points
;OUTPUT: A file with pixels to be set to a default value
;ROUTINES CALLED:
;MODIFICATION HISTORY: Jan 1990,June, 1993
;*********************************************************************
;
; Author: Clif Kirkpatrick, 2007-16-7
;
;-----------------------------------------------------------------------
;
; Check for correct number of parameters
;
   np=n_params(0)
   IF ((np lt 2) OR (np gt 2)) THEN BEGIN
      print,string(7B),'CALLING SEQUENCE: ', $
      'mkmask, image, maskout'
      RETURN   
   ENDIF



;
; Read in image and compute dimensions
;
   image=readfits(image)
   hobig=size(image)
   nxpix=hobig(1) &nypix=hobig(2)
   crunch=1
   nx=nxpix/crunch& ny=nypix/crunch
   image=congrid(image,nx,ny)



;
; Display image
;
   loadct,30 
   nofix=1  ;flag is true
   print,' '
   print,'Set contrast...'
   print,' '
   read,'      Low Density: ',lowval
   read,'     High Density: ',hival
   print,' '
   window,1,xsize=nx,ysize=ny
   tv,bytscl(image,lowval,hival)



;
; Find if output file already exists
;
   checkmask=findfile(maskout,count=num)



;
; Open up output file for writing
;
   IF (num eq 1) THEN BEGIN
      close,1
      openr,1,maskout
      FMT='I,I,I,I'
      readcol,maskout,F=FMT,DELIMITER='bdpix(:,)=badval',xl,xh,yh,yl
      nsize=SIZE(xl,/N_ELEMENTS)
      FOR i=0,nsize-1 DO BEGIN
         centx=(xh(i)+xl(i))/2
         centy=(yh(i)+yl(i))/2
         dx=xh(i)-xl(i)
         dy=yh(i)-yl(i)
         tvbox,[dx,dy],centx,centy,255
      ENDFOR
      close,1
      openw,1,maskout,/append
   ENDIF ELSE BEGIN
      close,1
      openw,1,maskout
   ENDELSE



;
; Begin mask creation loop
;
   REPEAT BEGIN
      nostars=1 ;flag is true
      xx=fltarr(2)  & yy=fltarr(2)
      xsc=fltarr(2) & ysc=fltarr(2)



;
; Change contrast of image and reload previous masks
;
      read,'Keep contrast? YES(0) NO(1): ',chgcon
      print,' '
      IF (chgcon eq 1) THEN BEGIN
         close,1
         openr,1,maskout
         print,' '
         print,'Set contrast...'
         print,' '
         read,'      Low Density: ',lowval
         read,'     High Density: ',hival
         window,1,xsize=nx,ysize=ny
         tv,bytscl(image,lowval,hival)
         FMT='I,I,I,I'
         readcol,maskout,F=FMT,DELIMITER='bdpix(:,)=badval',xl,xh,yh,yl
         nsize=SIZE(xl,/N_ELEMENTS)
         FOR i=0,nsize-1 DO BEGIN
            centx=(xh(i)+xl(i))/2
            centy=(yh(i)+yl(i))/2
            dx=xh(i)-xl(i)
            dy=yh(i)-yl(i)
            tvbox,[dx,dy],centx,centy,255
         ENDFOR
         close,1
         openw,1,maskout,/append
      ENDIF



;
; Define coordinates of mask
;
      print,'Locate object and draw box...'
      print,' '
      FOR j=0,1 DO BEGIN
         cursor,x,y,3,/device
         print,'x position'
         xx(j)=x
      ENDFOR
      FOR j=0,1 DO BEGIN
         cursor,x,y,3,/device
         print,'y positon'
         yy(j)=y 
      ENDFOR



;
; Draw boxes in IDL window
;
      boxx=[min(xx),max(xx),max(xx),min(xx),min(xx)]
      boxy=[min(yy),min(yy),max(yy),max(yy),min(yy)]    
      boxl=max(xx)-min(xx)
      boxw=max(yy)-min(yy)
      print,xx,yy
      xz=min(xx)+boxl/2.
      print,'xz=',xz
      yz=min(yy)+boxw/2.
      print,'yz=',yz
      boxlw=[boxl,boxw]
      boxlw=boxlw;   crunch      
      boxer,boxlw,xz,yz
      !c=0 



;
; 
;
      print,' '
      read,'OK? YES(0) NO(1): ',okk
      print,' '
      xx=fix(xx)
      yy=fix(yy)
      IF (okk eq 0) THEN printf,1,'bdpix(',min(xx)*crunch,':',max(xx)*crunch,',',$      
                             min(yy)*crunch,':',max(yy)*crunch,')=badval'
      read,'Create another region? YES(0) NO(1): ',nostars
      print,' '
   ENDREP UNTIL nostars
   close,1



;
; RETURN and END
;
   RETURN



END
