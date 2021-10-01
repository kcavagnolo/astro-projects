Pro SER_V2, evt, header, SKYX, SKYY, newchipx, newchipy, XYonly = XYonly, $
	 Corneronly=Corneronly, energyANDcharge = EC, static=static, $
         writetofile = writetofile, infile = infile, outfile = outfile
;;+
;;SER, version 2.0, avaliable from Feb. 2003
;; Subpixel Event Repositioning algorithms for 
;; both BI and FI CCDs. Four SER methods can be used here, i.e., 
;; Energy-dependent (default), Static (/static), 
;; Tsunemi method(/Corneronly), and Charge split dependent (/EnergyANDCharge)
;;
;; Different SER methods apply different event impact position offset at SKY
;; Coordinates. Shifts depend on the event grades, energy and charge split 
;; proportion, as well as CCD type (BI or FI).
;; The energy/charge dependent shifts are from the BI/FI CCD simulations.
;;
;; WHAT'S NEW OF THIS VERSION:
;; DO NOT NEED CCD_ID INPUT &
;; This program will reposition split Events for all ACIS CCDs.

;;INPUT:
;;	evt: The event list of a Chandra/ACIS observation LEVEL ONE (1) file 
;;	     AFTER removing randomization. The columns in the event structure
;;	     must include: ['CCD_ID', 'fltgrade', 'energy', 'x', 'y', 'phas']
;;	header: The header of the event list, which provide the information of
;;		the roll angle.
;;Optioinal Keyword:
;;	Static: Use energy-independent shift for the split events, 0.5/0.366(BI)
;;		or 0.5/0.47(FI) pixel shifts for Corner/2-pixel split events. 
;;		Default is energy dependent (and event grade dependent) SER. 
;;		The photon energies are limite to 0.3 to 12 keV.		
;;	EnergyAndCharge: 
;;		the shift is energy-dependent and charge split proportion
;;		depdendent. This will provide the most accurate SER correction.
;;	Corneronly: Only correct Corner Split events. This is the model of
;;		Tsunemi et al. (2001). The default is repositioning both Corner
;;		split and 2-pixel Split events.
;;	WritetoFile: Write the corrected event lists into a fits file (only Sky 
;;	        coordinates were corrected)
;;	infile: The input file, which was used for all the header information
;;	outfile: The output file, used for writing into the corrected event list
;;	XYonly: Only write Sky coordinates [X,Y] into the 'outfile'.
;;OUTPUT (optional):
;;	SKYX/Y: output arrays, have same size as evt.x/y, but with new locations
;;		for split events.
;;	NEWCHIPX/Y: Optional output of modified CHIP coordinates.

;;EXTERNAL CALLING FUNCTIONS OTHERS THAN ASTRO LIB:
;;	COSD: Cosine function with input angle as degrees
;;	SIND: Sine Function with input angel as degrees
;;	Event_reposition2: to relocate the event position by grade and shifts.
;;SIDE EFFECT:
;;	unknown
;;EXAMPLE
;;  evt = mrdfits('acis_evt1.fits', 1, header)
;;  SER_v2, evt, header, SKYx, SKYy, /static, /writetofile, /XYonly, $
;;	infile = 'acis_evt1.fits', outfile = 'acis_ser_evt1.fits'
;; History:
;; last modified at Oct. 15, 2003
;;-
ON_ERROR, 2 	;return to main when error occurs
start=systime(1)
;;Check input
If N_params() LT 2 then begin
  print, '% Wrong inputs. Usage as:  '
  message, 'SER, evt, header, [SKYX, SKYY, /static, /Corneronly, '+ $
  	    '/EnergyANDCentroid', +$
  	    '/writetofile, infile=<input file>, outfile = <output file>]'
EndIf
If N_elements(evt) EQ 0 Then $
  message, 'Wrong EVENT LIST input, check event list'
Rollang=sxpar(header, "ROLL_NOM", count = match)
If match EQ 0L Then $
  message, 'Invalid header, check the extension. Read it in ' + $
  'like->header=headfits(dir+filename,exten=1)'

;; The modified chip coordinates
If N_params() GT 6 then begin
   newchipx = float(evt.chipx)
   newchipy = float(evt.chipy)
Endif
;;readout the fits file	   
   findpro,'BIshifts.fits', /noprint, Prolist=PL
   if PL[0] eq '' then message, 'Cannot find the table'
   BIshifts = readfits(PL[0])
   eng = BIshifts[*,0]
   IV_pix = BIshifts[*,2]
   III_pix = BIshifts[*,3]
   II_pix = BIshifts[*,4]
if Keyword_set(STATIC) Then Begin
  IV_pix [*] = 0.5
  III_pix[*] = 0.5
  II_pix[*] = 0.366  
EndIf
;;	^CHIPY			^ -DETY
;;	|			|
;;	|			|
;;	|----------->CHIPX	-----------> DETX

;;FOR CCD_ID 0 & 2

;;	^CHIPY			^ DETX
;;	|			|
;;	|			|
;;	|----------->CHIPX	-----------> DETY

;;FOR CCD_ID 1 & 3

;;	^CHIPY			^ -DETX
;;	|			|
;;	|			|
;;	|----------->CHIPX	-----------> -DETY


SKYX=evt.X & SKYY=evt.Y 
  ;; Because of the different orentation, The CCD chips need adjust orentation angle.  
  ;; divide the CCD_ID into 4 groups
  ;; Group1: 5, 7 for BI CCD, angle=0
  ;; Group2: 4,6,8,9, FI, angle = 0
  ;; group3: 0, 2, FI, angle=90
  ;; Group4: 1, 3, FI, angle=-90
  
;;because of layout of different ACIS chips, the adjustPhi has to be different
;;  IF (cid eq 0 or cid eq 2) then adjustPhi=90 $
;;  Else if (cid eq 1 or cid eq 3) then adjustPhi=-90 
  
;;Group1, BI, angle=0
   adjustPhi=0 ;degrees for CCD 5, 7
   good=where(evt.CCD_ID eq 5 or evt.CCD_ID eq 7 and evt.energy GE 300 $
   	      and evt.energy LE 12000, cnt)
   if cnt EQ 0 Then begin
     print, 'There is no event on S1 and S3 (BI CCDs)'
     GOTO, GROUP2
   Endif  
;;call the procedure
   if keyword_set(EC) then $
   Event_reposition2, evt, skyx, skyy, good, rollang, adjustPhi, eng, $
	  IV_pix, III_pix, II_pix, newchipx, newchipy, /energyANDcentroid $
   Else $
   Event_reposition2, evt, skyx, skyy, good, rollang, adjustPhi, eng, $
	  IV_pix, III_pix, II_pix, newchipx, newchipy 
GROUP2:
;;Change SHIFTS:
if Keyword_set(STATIC) Then $
  II_pix[*] = 0.47 $;FOR FI devices 
Else Begin
  findpro,'FIshifts.fits', /noprint, Prolist=PL
  FIshifts = readfits(PL[0])
  if PL[0] eq '' then message, 'Cannot find the table'
  eng = FIshifts[*,0]
  IV_pix = FIshifts[*,2]  
  III_pix = FIshifts[*,3] 
  II_pix = FIshifts[*,4] 
  indx=where(IV_pix eq 0, cnt0)
  if cnt0 GT 0 then IV_pix(indx) = 0.48
  indx=where(III_pix eq 0, cnt0)
  if cnt0 GT 0 then III_pix(indx) = 0.48
  indx=where(II_pix eq 0, cnt0)
  if cnt0 GT 0 then II_pix(indx) = 0.48
EndElse

;; Group2: 4,6,8,9, FI, angle = 0
   adjustPhi=0 ;degrees for CCD 4,6,8,9
   good=where(evt.CCD_ID eq 4 or evt.CCD_ID eq 6 or evt.CCD_ID eq 8 or $
              evt.CCD_ID eq 6  and evt.energy GE 300 $
   	      and evt.energy LE 12000, cnt)
   if cnt EQ 0 Then begin
       print, 'There is no events on ACIS-S 0, 2, 4, 5 (FI CCDs)'
       GOTO, GROUP3
   Endif   
;;call the procedure
   Event_reposition2, evt, skyx, skyy, good, rollang, adjustPhi, eng, $
	  IV_pix, III_pix, II_pix, newchipx, newchipy, $
	  energyANDcentroid=EC,/FI

GROUP3:
;; group3: 0, 2, FI, angle=90
   adjustPhi=90 ;degrees for CCD 0,2
   good=where(evt.CCD_ID eq 0 or evt.CCD_ID eq 2 and evt.energy GE 300 $
   	      and evt.energy LE 12000, cnt)
   if cnt EQ 0 Then begin
       Print, 'There is no events on ACIS-I 0, 2 (FI CCDs).'
       GOTO, GROUP4
   Endif  
;;call the procedure
   Event_reposition2, evt, skyx, skyy, good, rollang, adjustPhi, eng, /FI, $
	  IV_pix, III_pix, II_pix, newchipx, newchipy, $
	  energyANDcentroid=EC

GROUP4:
;; Group4: 1, 3, FI, angle=-90   
   adjustPhi=-90 ;degrees for CCD 1,3
   good=where(evt.CCD_ID eq 1 or evt.CCD_ID eq 3 and evt.energy GE 300 $
   	      and evt.energy LE 12000, cnt)
   if cnt EQ 0 Then begin
         Print, 'There is no events on ACIS-I 1, 3 (FI CCDs).'
         GOTO, NEXTSTEP
   Endif
;;call the procedure
   Event_reposition2, evt, skyx, skyy, good, rollang, adjustPhi, eng, /FI, $
	  IV_pix, III_pix, II_pix, newchipx, newchipy, $
	  energyANDcentroid=EC

NEXTSTEP:
;;DONE
If keyword_set(CORNERONLY) Then begin  
   twopix = where (evt.fltgrade EQ 2 Or evt.fltgrade EQ 8 OR $
       		  evt.fltgrade EQ 16 OR evt.fltgrade EQ 64, cnt5)
   If (cnt5) GT 0 then begin    		       
       SKYX[twopix] = evt[twopix].x
       SKYY[twopix] = evt[twopix].y		       
   EndIf
ENDIF

print,'		The Calculation time is: ', systime(1)-start

If keyword_set(writetofile) then begin
   If not keyword_set(infile) then begin
      infile = ''
      read, infile, PROMPT ='Type in  the input file please:'     
   EndIf   
   if keyword_set(XYonly) then $
     evtout=replicate({x:0.0, y:0.0}, N_elements(Skyx)) $     
   Else evtout=evt
   evtout.x = SKYX
   evtout.y = SKYY
   primaryHDR  = headfits( infile )
   e1HDR = headfits( infile, EXTEN=1 )
   e2HDR = headfits( infile, EXTEN=2 )
   GTI = mrdfits( infile, 2 )
   If not keyword_set(outfile) then begin
      outfile = ''
      read, outfile, PROMPT ='Type in  the output (fits) file to write in:'
   ENDIF
   today = strmid(systime(0), 4, 6) +strmid(systime(0), 19, 5)
   SXADDPAR, primaryHDR, 'SER_MOD', 'T','SER ver 2.0, at '+ today
   writefits, outfile, 0, primaryHDR
   mwrfits, evtout, outfile, e1HDR
   mwrfits, GTI, outfile, e2HDR
EndIf
print,'		Total cost time is: ', systime(1)-start
End
