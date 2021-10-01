;+
;                       Display True Color Image
;*NAME:	disp_true_wid.pro
;
;*PURPOSE:  
;           Widget tool for displaying the True Color image within TCTOOL
;
;
;*CATEGORY:  Called by TCTOOL
;           
;
;*CALLING SEQUENCE:  disp_true_wid,post
;           
;
;*INPUTS:      post - a flag that indicates where the true color image goes.
;              
;              post=0 ---> display to True Color window in TCTOOL
;              post=1 ---> write a postscript file  (idl.ps)
;              post=2 ---> write a TIFF file  (idl.tiff)
;              post=3 ---> write a JPEG file  (idl.jpeg)
;
;*OUTPUTS:     NONE
;       
;
;*KEYWORD PARAMETERS:   NONE
;           
;
;
;
;*EXAMPLES:
;
;*PROCEDURE:  TCTOOL does all that is needed (post is defined by the
;             "WRITE IMAGE" pull down menu).
;          
;
;*SUPPORT PROCEDURES:  NONE
;
;
;*HISTORY:
;	6/  /99    	E Malumuth/RITSS
;       4/24/01		E Malumuth/RITSS  -  Modified the PostScript output to reflect the on-screen
;                                            arrows (N & E) and Annotations.  Note this only works
;                                            for PS output not TIFF or JPEG
;-
pro disp_true_wid,post
   COMMON values,red,green,blue,red_scale_type,green_scale_type,blue_scale_type
   COMMON scales,redmin,redmax,greenmin,greenmax,bluemin,bluemax,redshift, $
                 greenshift,blueshift
   COMMON DRAW14_Comm, DRAW14_Id
   COMMON images,redim,greenim,blueim
   COMMON timages,tredimage,tgreenimage,tblueimage
   COMMON output,outfile
   COMMON arrowvals,arrowflg
   COMMON rotvals, ima,ang,magn,xcenter,ycenter
   COMMON ASPECT, fxr,fyr,fxg,fyg,fxb,fyb
   COMMON headers,rhead,ghead,bhead
   common annotatevals,annotateflg,annxcent,annycent,anntext
;
;
if(red_scale_type eq 0) then begin
  tredimage=redim
  redimage=red
endif
if(green_scale_type eq 0) then begin
  tgreenimage=greenim
  greenimage=green
endif
if(blue_scale_type eq 0) then begin
  tblueimage=blueim
  blueimage=blue
endif

if(red_scale_type eq 1) then begin
  tredimage=alog10(redim>10^redmin)
  redimage=alog10(red>10^redmin)
endif
if(green_scale_type eq 1) then begin
  tgreenimage=alog10(greenim>10^greenmin)
  greenimage=alog10(green>10^greenmin)
endif
if(blue_scale_type eq 1) then begin
  tblueimage=alog10(blueim>10^bluemin)
  blueimage=alog10(blue>10^bluemin)
endif

if(red_scale_type eq 2) then begin
  tredimage=sqrt(redim>redmin^2)
  redimage=sqrt(red>redmin^2)
endif
if(green_scale_type eq 2) then begin
  tgreenimage=sqrt(greenim>greenmin^2)
  greenimage=sqrt(green>greenmin^2)
endif
if(blue_scale_type eq 2) then begin
  tblueimage=sqrt(blueim>bluemin^2)
  blueimage=sqrt(blue>bluemin^2)
endif

if(red_scale_type eq 3) then begin
  tredimage=hist_equal(redim)
  redimage=hist_equal(red)
endif
if(green_scale_type eq 3) then begin
  tgreenimage=hist_equal(greenim)
  greenimage=hist_equal(green)
endif
if(blue_scale_type eq 3) then begin
  tblueimage=hist_equal(blueim)
  blueimage=hist_equal(blue)
endif


if (post eq 0) then begin
wset, DRAW14_Id
tredimage=bytscl(tredimage,redmin,redmax)
tgreenimage=bytscl(tgreenimage,greenmin,greenmax)
tblueimage=bytscl(tblueimage,bluemin,bluemax)
tv,tredimage,channel=1
tv,tgreenimage,channel=2
tv,tblueimage,channel=3
endif
if (post eq 1) then begin
set_plot,'ps'
device,/in,xs=6.5,ys=6.5,xoff=1,yoff=2.25,/color,bits=8,filename=outfile+'.ps'
sr=size(redimage)
sg=size(greenimage)
sb=size(blueimage)
if (sr(1) eq sg(1) and sg(1) eq sb(1) and sr(2) eq sg(2) and sg(2) eq sb(2)) $
  then begin
  samesize=1
endif else begin
    redimage=congrid(redimage,600,600)
    greenimage=congrid(greenimage,600,600)
    blueimage=congrid(blueimage,600,600)
    samesize=0
endelse
tv,[[[bytscl(redimage,redmin,redmax)]], $
    [[bytscl(greenimage,greenmin,greenmax)]], $
    [[bytscl(blueimage,bluemin,bluemax)]]],true=3
if(arrowflg eq 1) then begin
   if(samesize eq 1) then begin
      arrows,rhead,(16510./sr(1))*xcenter,(16510./sr(2))*ycenter,color=255  
   endif else begin
      arrows,rhead,(16510./sr(1))*(xcenter/fxr),(16510./sr(2))*(ycenter/fyr),color=255
   endelse
endif
if(annotateflg eq 1) then begin
  fits_info,'annotate.fits',n_ext=nextend,/silent
  for iext=1,nextend do begin 
    st=mrdfits('annotate.fits',iext,h)
    p=st.p
    c= long(p[4])
    CASE p[19] of
    0: BEGIN
       str = '!' + strtrim(fix(p[13])>3,2) + st.txt + '!3'
       xyouts, p[0], p[1],/NORM,str,COLOR = c, CHARSIZE = p[7]*.725, $
           ALI = p[12], ORI=p[14], CHARTHICK = p[5], WIDTH=x
       END
    1: BEGIN
         IF (p[17] ne 0) THEN BEGIN
             IF p[15] le 0.0 then p[15] = 1.0
             ARROW, p[2], p[3], p[0], p[1], /NORM, $
		COLOR = c, THICK = p[5], SOLID=p[17] eq 2, $
		HSIZE = p[15] * !D.X_SIZE/20.
         ENDIF ELSE BEGIN
             PLOTS, p[[2,0]], p[[3,1]], /NORM, COLOR=c, THICK=p[5], $
		LINESTYLE=p[6]
         ENDELSE
       END
    ENDCASE
 endfor
endif
device,/close
spawn,'mv -f annotate.fits annotate_old.fits'
set_plot,'x'
endif
if (post eq 2) then begin
sr=size(redimage)
sg=size(greenimage)
sb=size(blueimage)
if (sr(1) eq sg(1) and sg(1) eq sb(1) and sr(2) eq sg(2) and sg(2) eq sb(2)) $
  then begin
    tim=bytarr(3,sr(1),sr(2))
endif else begin
    redimage=congrid(redimage,600,600)
    greenimage=congrid(greenimage,600,600)
    blueimage=congrid(blueimage,600,600)
    tim=bytarr(3,600,600)
endelse
tim(0,*,*)=rotate(bytscl(redimage,redmin,redmax),7)
tim(1,*,*)=rotate(bytscl(greenimage,greenmin,greenmax),7)
tim(2,*,*)=rotate(bytscl(blueimage,bluemin,bluemax),7)
write_tiff,outfile+'.tiff',tim,1
endif
if (post eq 3) then begin
sr=size(redimage)
sg=size(greenimage)
sb=size(blueimage)
if (sr(1) eq sg(1) and sg(1) eq sb(1) and sr(2) eq sg(2) and sg(2) eq sb(2)) $
  then begin
    tim=bytarr(3,sr(1),sr(2))
endif else begin
    redimage=congrid(redimage,600,600)
    greenimage=congrid(greenimage,600,600)
    blueimage=congrid(blueimage,600,600)
    tim=bytarr(3,600,600)
endelse
tim(0,*,*)=bytscl(redimage,redmin,redmax)
tim(1,*,*)=bytscl(greenimage,greenmin,greenmax)
tim(2,*,*)=bytscl(blueimage,bluemin,bluemax)
write_jpeg,outfile+'.jpeg',tim,true=1,quality=100
endif
return
end


