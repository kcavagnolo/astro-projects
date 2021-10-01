pro BScentrd,img,xgess,ygess,x,y,FWHM1,INFO=INFO
;+
; NAME: 
;   BSCENTRD
; PURPOSE:
;   Compute "center of mass" centroid coordinates of a stellar
;   object.  This
;   procedure is considerable slower than the Astronomy libraries'
;   CNTRD
;   procedure, but yields much better results for small "BAD STARS",
;   i.e.
;   small scruffy objects which don't necessarily have a good shape.
;   It also
;   avoids the CNTRD problem of ignoring a small star next to a much
;   brighter
;   one even if the exact coordinates are supplied.  Results seem to
;   be of
;   comperable accuracy (based on comaprisons of residuals when
;   transferring
;   solutions to higher resolution images) on bright sources and can
;   measure
;   many sources CNTRD is not capable of.  Careful study of results of
;   this
;   algorithm versus other popular ones still needs to be done.  In
;   general,
;   results are quite good, actually.  The author uses is for ALL
;   measurement
;   work, not just "BAD STARS".
; CALLING SEQUENCE: 
;   BSCENTRD,img,xguess,yguess,xcen,ycen,[FWHM],[/INFO]
; INPUTS:     
;   IMG      Two dimensional image array
;   XGUESS   Scalar giving approximate X stellar center
;   YGUESS   Scalar giving approximate Y stellar center
; OPTIONAL OUTPUT:
;   FWHM     Returned approximate FWHM.  Not terribly accurate.  Needs
;   work.
; OUTPUTS:   
;   XCEN     The computed X centroid position.  -1 if unable to
;   centroid
;   YCEN     The computed Y centroid position.  -1 if unable to
;   centroid
;  OPTIONAL OUTPUT KEYWORDS:
;   INFO     If set, BScentrd prints out some informational
;   statistics.  They
;              are pretty good for small stars but not for large
;              stars.
;              This needs some work, too.
; PROCEDURE: 
;   Nearest peak to the specified GUESS coordinates is determined.
;   Appriximate
;   extent of star is determined and nearby stars are masked out.
;   Center of
;   "mass" of the remaining star information is returned.
; MODIFICATION HISTORY:
;   06-JUN-90 Written by Eric Deutsch
;             numerous undocumented adjustments made over time
;   04-APR-93 Header spiffed up for release.  EWD.
;-


  if (n_params(0) lt 5) then begin
    print,"Call> BScentrd,img,xguess,yguess,xcen,ycen,[fwhm1,info=info]"
    print,"e.g.> BScentrd,img,420,510,xcen,ycen"
    return
endif

  if (n_elements(INFO) eq 0) then INFO=0


  WIDTH=15
  EXPAND=10
  
START:
  xguess=fix(xgess) & yguess=fix(ygess)
  im1=extrac(img,xguess-WIDTH/2,yguess-WIDTH/2,WIDTH,WIDTH)*1.
  im2=congrid(im1,WIDTH*EXPAND,WIDTH*EXPAND)
  im3=smooth(smooth(im2,WIDTH/2),WIDTH/2)
  av=avg(im3) 
  bak=avg(im3<av)
  if (INFO eq 1) then print,'Background Value: ',strn(bak)
  im4=(im3-bak)>0
  W=WIDTH*EXPAND & C=W/2 & D=W*.2

; ***** Find peak near center
; *************************************************
  prevmax=0 & curmax=0 & inarow=0 & cx=0 & cy=0 & i=0
  while (cx eq 0) and (i lt C/2) do begin
    curmax=max(im4(C-i:C+i,C-i:C+i))
    if (curmax eq prevmax) then inarow=inarow+1 $
    else begin prevmax=curmax & inarow=0 & endelse
    if (inarow eq 5) then begin
      cy=where(im4 eq curmax)/(WIDTH*EXPAND)
      cx=where(im4 eq curmax)-cy*(WIDTH*EXPAND)
      cx=cx(0) & cy=cy(0)
  endif
    i=i+1
endwhile

  if (cx eq 0) then begin
    x=-1 & y=-1 & print,'Unable to Centroid Star: No peak found.' & return
endif
  if (abs(cx-C) gt 3*EXPAND) or (abs(cy-C) gt 3*EXPAND) then begin
    xgess=xgess+(cx-C)/EXPAND & ygess=ygess+(cy-C)/EXPAND
    print,'Repositioning to ',vect([xgess,ygess])
    goto,START
endif

; ***** Find good box to contain star
; *****************************************
  prevtot=0 & curtot=0 & i=0 & blank=0 & boxdiff=fltarr(50)
  while (i lt 50) do begin
    curtot=total(im4((cx-i)>0:(cx+i)<(W-1),(cy-i)>0:(cy+i)<(W-1)))
    boxdiff(i)=curtot-prevtot
    prevtot=curtot
    i=i+1
endwhile
  boxdiff=smooth(boxdiff,2)
  slopearr=(boxdiff(1:49)-boxdiff(0:48))/max(boxdiff)*20*5
;  plot,slopearr
;  key=get_kbrd(1)
  i=0 & slope=0. & flag=0
  while (i lt 49) and (flag lt 4) do begin
    slope=boxdiff(i+1)-boxdiff(i)
    if (flag lt 2) then begin
      if (slope lt 0) then flag=flag+1 else flag=0
  endif
    if (flag gt 1) and (flag lt 4) then begin
      if (slope gt 0) then flag=flag+1 else flag=2
  endif
    i=i+1
endwhile
  i=i-2

; ***** Remove any stuff outside box
; ******************************************
  start=[cy+i,cy-i,cx+i,cx-i] & finish=[W-1,0,W-1,0] & direc=[1,-1,1,-1]
  for dir=0,3 do begin
    blank=1
    for i=start(dir),finish(dir),direc(dir) do begin
      if (blank eq 1) then begin
        if (dir lt 2) then im4(*,i)=0 else im4(i,*)=0
    endif
endfor
endfor

; ***** Reconstruct original box
; **********************************************

  im1=(im1-bak)>0
  for i=0,width-1 do begin
    for j=0,width-1 do begin
      tmp=extrac(im4,j*expand,i*expand,expand,expand)
      zer=where(tmp eq 0)
      if (n_elements(zer) gt 4) then begin
        im1(j,i)=im1(j,i)*(n_elements(zer)/WIDTH^2)
    endif
;      if (n_elements(zer) gt expand*expand*.6) then begin
;        im2(j*expand:j*expand+expand-1,i*expand:i*expand+expand-1)=0
endfor
endfor

  if (INFO eq 1) then begin
    openw,1,'BScentrd.dmp'
    printf,1,fix(im1(*,WIDTH-1-indgen(WIDTH))+.5)
    close,1
;    surface,im4
endif

; ***** Calculate some goodies about the star
; *********************************

  if (INFO eq 1) then begin
    hmx=max(im1)/2. & FW=intarr(4) & FWHM1=FW
    xd=[1,0,-1,0] & yd=[0,1,0,-1]
    for i=0,C do begin
      for j=0,3 do begin
        x=cx+xd(j)*i & y=cy+yd(j)*i
        if ((x ge 0) and (y ge 0) and (x lt W) and (y lt W)) then begin
          if (FW(j) eq 0) and (im4(x,y) eq 0) then FW(j)=i
          if (FWHM1(j) eq 0) and (im4(x,y) lt hmx) then FWHM1(j)=i
      endif
  endfor
endfor
    print,'Full Width: ',strn(avg(FW)/EXPAND),'   RMS: ', $
      strn(stdev(FW)/EXPAND),'        FW: ',vect(FW)
    print,'Full Width Half Max: ',strn(avg(FWHM1)/EXPAND),'   RMS: ', $
      strn(stdev(FWHM1)/EXPAND),'        FWHM: ',vect(FWHM1)
    FWHM1=avg(FWHM1)*1./EXPAND
endif

; ***** Calculate the center of Volume of the star
; ****************************

  sttot=1.0D*total(im1) & i=0 & xcen=0. & tot=0.
  while (xcen eq 0) and (i lt WIDTH) do begin
    band=im1(i,*) & btot=total(band)
    if (tot+btot gt sttot/2.) then xcen=i-.5+(sttot/2-tot)/btot
    i=i+1 & tot=tot+btot
endwhile
  i=0 & ycen=0. & tot=0.
  while (ycen eq 0) and (i lt WIDTH) do begin
    band=im1(*,i) & btot=total(band)
    if (tot+btot gt sttot/2.) then ycen=i-.5+(sttot/2-tot)/btot
    i=i+1 & tot=tot+btot
endwhile

  x=xguess-WIDTH/2+xcen
  y=yguess-WIDTH/2+ycen

  return
end
