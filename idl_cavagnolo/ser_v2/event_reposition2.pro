PRO Event_reposition2, EVT, SKYX, SKYY, good, rollang, adjustPhi, eng, $
	IV_pix, III_pix, II_pix, chipx, chipy, $
	energyANDcentroid = EC, FI = FI
	
;; This program is designed only as SER_V2 subrutine.
;; INPUTS:
;; EVT:	  The Event List of the who observation
;; SKYX:  The input/output of the event repositioned SKYX location
;; SKYY:  The input/output of the event repositioned SKYY location
;; NOTE   THAT SKYX and SKYY will change after calling this procedure.
;; Good:  The index
;; Rollang: Roll angle of the spacecraft.
;; adjustPhi: The adjusted angle for different CCD chips.
;; eng:   The energy vector from 0.3 to 12 keV.
;; IV_pix:  the offset vector for 4-pixel split events.
;; III_pix: the offset vector for 3-pixel split events.
;; II_pix:  the offset vector for 2-pixel split events.
;; CHIPX/Y: The input/output of the event repositioned CHIPX/Y values
;; Keywords:
;;	EnergyANDCentroid: 
;;	if this is set, use energy and split-charge-proportion-dependent SER.
;;	FI: The front-illuminated Devices, default is BI

;; The grade formation is defined as:
;% 3x3 pixel neighborhood:
;%
;%      ._______________________.
;%      |       |       |       |
;%      |  32   |   64  |  128  |   <---- fltgrade bit
;%      |  2,0  |   2,1 |  2,2  |   <---- phas[3,3] i,j
;%      |-------+-------+-------|
;%      |       |       |       |
;%      |   8   |   0   |  16   |
;%      |  1,0  |  1,1  |  1,2  |
;%  ^   |-------+-------+-------|
;%  |   |       |       |       |
;%  y   |  1    |   2   |   4   |
;%      |  0,0  |  0,1  |  0,2  |
;%      .-----------------------.
;%   
;%   chipx --->
;; Split directions are defined as (in this program):
;;
;;		       (UP)
;;        (LeftUp)	^	(RightUP)
;;		\	|	/
;;		     \	|  /
;;   (Left)<---- (centra pixel) ---->(Right) 
;;		     /	|  \	
;;		/	|	\
;;      (LeftDown)     	v    	 (RightDown)
;;		      (DOWN)

;; Last modified at Feb. 10, 2003
;; -

;;Cosine and Sine constant:
coscons = cos((-Rollang+adjustPhi)*!pi/180.0)
sincons = sin((-Rollang+adjustPhi)*!pi/180.0)

fltgrade=evt(good).fltgrade
if keyword_set(EC) then begin ; calculate the split proportion
  ;;Read out the parameter's file, according to BI and FI devices:
  findpro, 'BIshifts_ppt.fits'
  if PL[0] eq '' then message, 'Cannot find the table'
  BIpar = readfits(PL[0])
  findpro, 'FIshifts_ppt.fits'
  if PL[0] eq '' then message, 'Cannot find the table'  
  FIpar = readfits(PL[0])
  print,'EVENT REPOSITIONING, Energy and Charge Split Proportion'
  If keyword_set(FI) then par = FIpar $
  Else par = BIpar
  data = evt(good).phas
  data(where(data lt 13)) = 0 
  portX = fltarr(N_elements(good)) & portY = portX
  modify = where(fltgrade eq 10 or fltgrade eq 11 or fltgrade eq 18 or $
  		fltgrade eq 22 or fltgrade eq 72 OR fltgrade eq 104 or $
  		fltgrade eq 80 OR fltgrade eq 208 or fltgrade eq 2 or $
  		fltgrade eq 8 OR fltgrade eq 16 or fltgrade eq 64, cnt)
  if cnt GT 0 Then $
  for i=0L, cnt-1 do begin
    a = data[*,*,[modify[i]]]
    ttl_a = total(a)
    portX[modify[i]] = (a[0,1] + a[2,1])/ttl_a
    portY[modify[i]] = (a[1,0] + a[1,2])/ttl_a
  Endfor
Endif
if N_elements(CHIPX) NE 0 and N_elements(CHIPY) NE 0 then $
   cal_chip = 1B Else cal_chip = 0B
print, "ROLLANGLE IS: ", rollang
leftdown=where(fltgrade eq 10, cnt1)	;leftdown =>x- y+
if cnt1 GT 0 then begin
   If keyword_set(EC) then begin
     ceo0 = interpol(par[*,3], eng, evt(good(leftdown)).energy/1000.)
     ceo1 = interpol(par[*,4], eng, evt(good(leftdown)).energy/1000.)
     ceo2 = interpol(par[*,5], eng, evt(good(leftdown)).energy/1000.)
     DeltaX = -(0.5-(ceo0 + ceo1*portX(leftdown) + ceo2*(portX(leftdown))^2))    
     DeltaY = +(0.5-(ceo0 + ceo1*portY(leftdown) + ceo2*(portY(leftdown))^2))
   Endif Else begin
     DeltaX = -interpol(III_pix, eng, evt(good(leftdown)).energy/1000.);,;/spline)
     DeltaY = -DeltaX
   EndElse
   SKYX(good(leftdown))=evt(good(leftdown)).X+DeltaX*CosCons-DeltaY*SinCons
   SKYY(good(leftdown))=evt(good(leftdown)).Y+DeltaY*CosCons+DeltaX*SinCons
   if cal_chip then begin
      CHIPX(good(leftdown)) = CHIPX(good(leftdown)) + DeltaX
      CHIPY(good(leftdown)) = CHIPY(good(leftdown)) + DeltaY
   Endif
endif  
leftdown=where(fltgrade eq 11, cnt2)	;leftdown
if cnt2 GT 0 then begin
   If keyword_set(EC) then begin
     ceo0 = interpol(par[*,6], eng, evt(good(leftdown)).energy/1000.)
     ceo1 = interpol(par[*,7], eng, evt(good(leftdown)).energy/1000.)
     ceo2 = interpol(par[*,8], eng, evt(good(leftdown)).energy/1000.)
     DeltaX = -(0.5-(ceo0 + ceo1*portX(leftdown) + ceo2*(portX(leftdown))^2))    
     DeltaY = +(0.5-(ceo0 + ceo1*portY(leftdown) + ceo2*(portY(leftdown))^2))
   Endif Else begin
     DeltaX = -interpol(IV_pix, eng, evt(good(leftdown)).energy/1000.);,;/spline)
     DeltaY = -DeltaX
   EndElse

   SKYX(good(leftdown))=evt(good(leftdown)).X+DeltaX*CosCons-DeltaY*SinCons
   SKYY(good(leftdown))=evt(good(leftdown)).Y+DeltaY*CosCons+DeltaX*SinCons
   if cal_chip then begin
      CHIPX(good(leftdown)) = CHIPX(good(leftdown)) + DeltaX
      CHIPY(good(leftdown)) = CHIPY(good(leftdown)) + DeltaY
   Endif
endif

;;Since DeltaCHIPY is different w/ DetaDETX, so
;;flip over LeftDown w/ LeftUP and Rightdown w/ RightUP
;;Looks Sky coord. should shift the opposite way. Now add 180 degrees.

rightdown=where(fltgrade eq 18, cnt2)	;rightdown => X+ y+
;;shift right down
If cnt2 GT 0 Then begin
   If keyword_set(EC) then begin
     ceo0 = interpol_sample(par[*,3], eng, evt(good(rightdown)).energy/1000.)
     ceo1 = interpol_sample(par[*,4], eng, evt(good(rightdown)).energy/1000.)
     ceo2 = interpol_sample(par[*,5], eng, evt(good(rightdown)).energy/1000.)
     DeltaX = +(0.5-(ceo0 + ceo1*portX(rightdown) + ceo2*(portX(rightdown))^2))    
     DeltaY = +(0.5-(ceo0 + ceo1*portY(rightdown) + ceo2*(portY(rightdown))^2))
   Endif Else begin
     DeltaX = interpol(III_pix, eng, evt(good(rightdown)).energy/1000.);,;/spline)
     DeltaY = DeltaX
   EndElse
    SKYX(good(rightdown))=evt(good(rightdown)).X+DeltaX*CosCons-DeltaY*SinCons
    SKYY(good(rightdown))=evt(good(rightdown)).Y+DeltaY*CosCons+DeltaX*SinCons
    if cal_chip then begin
      CHIPX(good(rightdown)) = CHIPX(good(rightdown)) + DeltaX
      CHIPY(good(rightdown)) = CHIPY(good(rightdown)) + DeltaY
   Endif
Endif
rightdown=where(fltgrade eq 22 ,cnt2)	;rightdown
;;shift right down
If cnt2 GT 0 Then begin
   If keyword_set(EC) then begin
     ceo0 = interpol_sample(par[*,6], eng, evt(good(rightdown)).energy/1000.)
     ceo1 = interpol_sample(par[*,7], eng, evt(good(rightdown)).energy/1000.)
     ceo2 = interpol_sample(par[*,8], eng, evt(good(rightdown)).energy/1000.)
     DeltaX = +(0.5-(ceo0 + ceo1*portX(rightdown) + ceo2*(portX(rightdown))^2))    
     DeltaY = +(0.5-(ceo0 + ceo1*portY(rightdown) + ceo2*(portY(rightdown))^2))
   Endif Else begin
     DeltaX = interpol(IV_pix, eng, evt(good(rightdown)).energy/1000.);,;/spline)
     DeltaY = DeltaX
   EndElse
    SKYX(good(rightdown))=evt(good(rightdown)).X+DeltaX*CosCons-DeltaY*SinCons
    SKYY(good(rightdown))=evt(good(rightdown)).Y+DeltaY*CosCons+DeltaX*SinCons
    if cal_chip then begin
      CHIPX(good(rightdown)) = CHIPX(good(rightdown)) + DeltaX
      CHIPY(good(rightdown)) = CHIPY(good(rightdown)) + DeltaY
   Endif
Endif

leftUP=where(fltgrade eq 72 ,cnt3)	;leftup => x- y-
;;shift left up
If cnt3 GT 0 Then begin
   If keyword_set(EC) then begin
     ceo0 = interpol_sample(par[*,3], eng, evt(good(leftUP)).energy/1000.)
     ceo1 = interpol_sample(par[*,4], eng, evt(good(leftUP)).energy/1000.)
     ceo2 = interpol_sample(par[*,5], eng, evt(good(leftUP)).energy/1000.)
     DeltaX = -(0.5-(ceo0 + ceo1*portX(leftUP) + ceo2*(portX(leftUP))^2))    
     DeltaY = -(0.5-(ceo0 + ceo1*portY(leftUP) + ceo2*(portY(leftUP))^2))
   Endif Else begin
     DeltaX = -interpol(III_pix, eng, evt(good(leftUP)).energy/1000.);,;/spline)
     DeltaY = DeltaX
   EndElse
    SKYX(good(leftup))=evt(good(leftup)).X+DeltaX*CosCons-DeltaY*SinCons
    SKYY(good(leftup))=evt(good(leftup)).Y+DeltaY*CosCons+DeltaX*SinCons
    if cal_chip then begin
      CHIPX(good(leftup)) = CHIPX(good(leftup)) + DeltaX
      CHIPY(good(leftup)) = CHIPY(good(leftup)) + DeltaY
   Endif
Endif
leftUP=where(fltgrade eq 104 ,cnt3)	;leftup
If cnt3 GT 0 Then begin
    If keyword_set(EC) then begin
     ceo0 = interpol_sample(par[*,6], eng, evt(good(leftUP)).energy/1000.)
     ceo1 = interpol_sample(par[*,7], eng, evt(good(leftUP)).energy/1000.)
     ceo2 = interpol_sample(par[*,8], eng, evt(good(leftUP)).energy/1000.)
     DeltaX = -(0.5-(ceo0 + ceo1*portX(leftUP) + ceo2*(portX(leftUP))^2))    
     DeltaY = -(0.5-(ceo0 + ceo1*portY(leftUP) + ceo2*(portY(leftUP))^2))
   Endif Else begin
     DeltaX = -interpol(IV_pix, eng, evt(good(leftUP)).energy/1000.);,;/spline)
     DeltaY = DeltaX
   EndElse 
    SKYX(good(leftup))=evt(good(leftup)).X+DeltaX*CosCons-DeltaY*SinCons
    SKYY(good(leftup))=evt(good(leftup)).Y+DeltaY*CosCons+DeltaX*SinCons
    if cal_chip then begin
      CHIPX(good(leftup)) = CHIPX(good(leftup)) + DeltaX
      CHIPY(good(leftup)) = CHIPY(good(leftup)) + DeltaY
   Endif
Endif

rightup=where(fltgrade eq 80 ,cnt4)	;rightup => x+ y-
If cnt4 GT 0 Then begin
;;shift right up
   If keyword_set(EC) then begin
     ceo0 = interpol_sample(par[*,3], eng, evt(good(rightup)).energy/1000.)
     ceo1 = interpol_sample(par[*,4], eng, evt(good(rightup)).energy/1000.)
     ceo2 = interpol_sample(par[*,5], eng, evt(good(rightup)).energy/1000.)
     DeltaX = (0.5-(ceo0 + ceo1*portX(rightup) + ceo2*(portX(rightup))^2))    
     DeltaY = -(0.5-(ceo0 + ceo1*portY(rightup) + ceo2*(portY(rightup))^2))
   Endif Else begin
     DeltaX = interpol(III_pix, eng, evt(good(rightup)).energy/1000.);,;/spline)
     DeltaY = -DeltaX
   EndElse
    SKYX(good(rightup))=evt(good(rightup)).X+DeltaX*CosCons-DeltaY*SinCons
    SKYY(good(rightup))=evt(good(rightup)).Y+DeltaY*CosCons+DeltaX*SinCons
    if cal_chip then begin
      CHIPX(good(rightup)) = CHIPX(good(rightup)) + DeltaX
      CHIPY(good(rightup)) = CHIPY(good(rightup)) + DeltaY
   Endif
Endif
rightup=where(fltgrade eq 208 ,cnt4)	;rightup
;;shift right up
If cnt4 GT 0 Then begin
   If keyword_set(EC) then begin
     ceo0 = interpol_sample(par[*,6], eng, evt(good(rightup)).energy/1000.)
     ceo1 = interpol_sample(par[*,7], eng, evt(good(rightup)).energy/1000.)
     ceo2 = interpol_sample(par[*,8], eng, evt(good(rightup)).energy/1000.)
     DeltaX = (0.5-(ceo0 + ceo1*portX(rightup) + ceo2*(portX(rightup))^2))    
     DeltaY = -(0.5-(ceo0 + ceo1*portY(rightup) + ceo2*(portY(rightup))^2))
   Endif Else begin
     DeltaX = interpol(IV_pix, eng, evt(good(rightup)).energy/1000.);,;/spline)
     DeltaY = -DeltaX
   EndElse
    SKYX(good(rightup))=evt(good(rightup)).X+DeltaX*CosCons-DeltaY*SinCons
    SKYY(good(rightup))=evt(good(rightup)).Y+DeltaY*CosCons+DeltaX*SinCons
    if cal_chip then begin
      CHIPX(good(rightup)) = CHIPX(good(rightup)) + DeltaX
      CHIPY(good(rightup)) = CHIPY(good(rightup)) + DeltaY
   Endif
Endif

;;TWO-PIXEL SPLIT EVENTS

DOWN=where(fltgrade EQ 2,cnt5)		;DOWN 2-pix SPLIT events =>y+
If cnt5 GT 0 Then Begin
   If keyword_set(EC) then begin
     ceo0 = interpol_sample(par[*,0], eng, evt(good(DOWN)).energy/1000.)
     ceo1 = interpol_sample(par[*,1], eng, evt(good(DOWN)).energy/1000.)
     ceo2 = interpol_sample(par[*,2], eng, evt(good(DOWN)).energy/1000.)  
     DeltaY = +(0.5-(ceo0 + ceo1*portY(DOWN) + ceo2*(portY(DOWN))^2))
   Endif Else begin
     DeltaY = interpol(II_pix, eng, evt(good(DOWN)).energy/1000.);,;/spline)
   EndElse
   SKYX(good(DOWN))=evt(good(DOWN)).X-DeltaY*SinCons
   SKYY(good(DOWN))=evt(good(DOWN)).Y+DeltaY*CosCons
   if cal_chip then $
       CHIPY(good(DOWN)) = CHIPY(good(DOWN)) + DeltaY
Endif
LEFT=where(fltgrade EQ 8,cnt6)		;LEFT 2-pix SPLIT events =>x-
If cnt6 GT 0 Then Begin
   If keyword_set(EC) then begin
     ceo0 = interpol_sample(par[*,0], eng, evt(good(LEFT)).energy/1000.)
     ceo1 = interpol_sample(par[*,1], eng, evt(good(LEFT)).energy/1000.)
     ceo2 = interpol_sample(par[*,2], eng, evt(good(LEFT)).energy/1000.)
     DeltaX = -(0.5-(ceo0 + ceo1*portX(LEFT) + ceo2*(portX(LEFT))^2))
   Endif Else begin
     DeltaX = -interpol(II_pix, eng, evt(good(LEFT)).energy/1000.);,;/spline)
   EndElse
   SKYX(good(LEFT))=evt(good(LEFT)).X+DeltaX*CosCons
   SKYY(good(LEFT))=evt(good(LEFT)).Y+DeltaX*SinCons
   if cal_chip then $
       CHIPX(good(LEFT)) = CHIPX(good(LEFT)) + DeltaX
Endif
UPUP=where(fltgrade EQ 64,cnt7)		;UP 2-pix SPLIT events =>y-
If cnt7 GT 0 Then Begin
   If keyword_set(EC) then begin
     ceo0 = interpol_sample(par[*,0], eng, evt(good(UPUP)).energy/1000.)
     ceo1 = interpol_sample(par[*,1], eng, evt(good(UPUP)).energy/1000.)
     ceo2 = interpol_sample(par[*,2], eng, evt(good(UPUP)).energy/1000.)    
     DeltaY = -(0.5-(ceo0 + ceo1*portY(UPUP) + ceo2*(portY(UPUP))^2))
   Endif Else begin
     DeltaY = -interpol(II_pix, eng, evt(good(UPUP)).energy/1000.);,;/spline)
   EndElse
   SKYX(good(UPUP))=evt(good(UPUP)).X-DeltaY*SinCons
   SKYY(good(UPUP))=evt(good(UPUP)).Y+DeltaY*CosCons
   if cal_chip then $
       CHIPY(good(UPUP)) = CHIPY(good(UPUP)) + DeltaY
Endif
RIGHT=where(fltgrade EQ 16,cnt8)	;RIGHT 2-pix SPLIT events=>x+
If cnt8 GT 0 Then Begin
   If keyword_set(EC) then begin
     ceo0 = interpol_sample(par[*,0], eng, evt(good(RIGHT)).energy/1000.)
     ceo1 = interpol_sample(par[*,1], eng, evt(good(RIGHT)).energy/1000.)
     ceo2 = interpol_sample(par[*,2], eng, evt(good(RIGHT)).energy/1000.)
     DeltaX = (0.5-(ceo0 + ceo1*portX(RIGHT) + ceo2*(portX(RIGHT))^2))
   Endif Else begin
     DeltaX = interpol(II_pix, eng, evt(good(RIGHT)).energy/1000.);,;/spline)
   EndElse

   SKYX(good(RIGHT))=evt(good(RIGHT)).X+DeltaX*CosCons
   SKYY(good(RIGHT))=evt(good(RIGHT)).Y+DeltaX*SinCons
   if cal_chip then $
       CHIPX(good(RIGHT)) = CHIPX(good(RIGHT)) + DeltaX
Endif

END
