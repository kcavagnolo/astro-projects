pro find_hot_col,ievtfile,ibpixfile,oevtfile=oevtfile,obpixfile=obpixfile, $
	cutoff=cutoff, plotfile=plotfile
;-----------------------------------------------------------------------
; Name: FIND_HOT_COL
;
; Purpose: 
;          
; Inputs: 
;         
;         
; Comments: 
;           
;           
; Revision history:
;       written by Michael Wise and A. Hicks		06-20-00
;	modified to allow unique cutoffs for		09-11-00
;	each CCD (mw)
;
;-----------------------------------------------------------------------
;
; Check for correct number of parameters
;
np=n_params(0)
if ((np lt 2) or (np gt 6)) then begin
   print,string(7B),'CALLING SEQUENCE: ', $
   'find_hot_col,ievtfile,ibpixfile,oevtfile=oevtfile',$
   '    obpixfile=obpixfile,cutoff=cutoff,plotfile=plotfile'
   return   
endif

;
; Set default event output filename
;
if (n_elements(oevtfile) eq 0) then oevtfile='temp_evt2.fits'


;
; Set default bpix output filename
;
if (n_elements(obpixfile) eq 0) then obpixfile='out_bpix.fits'   


;
; Set default rejection tolerance
;
if (n_elements(cutoff) eq 0) then cutoff=5.0    ; Signal to noise ratio


;
; Set default name for output plotfile
;
if (n_elements(plotfile) eq 0) then plotfile='bad_columns.ps'


;
; Set default output text file name 
;
txtfile='bad_columns.txt'

;
; Open output text file
;
get_lun,unit
openw,unit,txtfile
blank='                    '
printf,unit,' '
printf,unit,'Removed hot columns from event file, updated bpix file'
printf,unit,' '
printf,unit,'Date: ',!stime
printf,unit,' '
printf,unit,'Input events file: ',ievtfile
printf,unit,'Input bpix file: ',ibpixfile
printf,unit,'Output events file: ',oevtfile
printf,unit,'Output bpix file: ',obpixfile
printf,unit,'SNR cutoff threshold: ',strcompress(cutoff)
printf,unit,' '
printf,unit,'    CCD_ID','     CHIPX'
printf,unit,'    ------','     -----'
   

;
; Read in the input EVENTS file
;
d0=mrdfits(ievtfile,0,hd0)	; Primary header	
d1=mrdfits(ievtfile,1,hd1)	; EVENTS extension
d2=mrdfits(ievtfile,2,hd2)	; GTI extension
d3=mrdfits(ievtfile,3,hd3)	; GTI extension
d4=mrdfits(ievtfile,4,hd4)	; GTI extension
d5=mrdfits(ievtfile,5,hd5)	; GTI extension
d6=mrdfits(ievtfile,6,hd6)	; GTI extension
d7=mrdfits(ievtfile,7,hd7)	; GTI extension


;
; Create event output file
;
mwrfits,d0,oevtfile,hd0


;
; Read in the input BPIX file
;
b0=mrdfits(ibpixfile,0,h0,structyp='bpix')	; Primary header	
b1=mrdfits(ibpixfile,1,h1,structyp='bpix')	; BPIX extension
b2=mrdfits(ibpixfile,2,h2,structyp='bpix')	; BPIX extension
b3=mrdfits(ibpixfile,3,h3,structyp='bpix')	; BPIX extension
b4=mrdfits(ibpixfile,4,h4,structyp='bpix')	; BPIX extension
b5=mrdfits(ibpixfile,5,h5,structyp='bpix')	; BPIX extension
b6=mrdfits(ibpixfile,6,h6,structyp='bpix')	; BPIX extension


;
; Read in ccd_id numbers
;
id1=fxpar(h1,'ccd_id')
id2=fxpar(h2,'ccd_id')
id3=fxpar(h3,'ccd_id')
id4=fxpar(h4,'ccd_id')
id5=fxpar(h5,'ccd_id')
id6=fxpar(h6,'ccd_id')



;
; Create bpix output file
;
sxaddpar,h0,'HISTORY',' Hot columns added '

mwrfits,b0,obpixfile,h0



;
; Determine which CCD's are on
;
plothist,d1.ccd_id,chips,dum,/noplot
ig=where(dum gt 0)
chips=fix(chips(ig))
nchips=n_elements(chips)



;
; Set default rejection tolerance
;
if (n_elements(cutoff) eq 1) then begin
   cutoff=replicate(cutoff,nchips)
endif else begin
;
;  Make sure input cutoff array is commiserate 
;  with number of CCDs
;
   if (n_elements(cutoff) ne nchips) then begin
      print,string(7B),$
      'ERROR: Length of CUTOFF array must match number of CCDs'
      return   
   endif
endelse



;
; Create arrays to hold flagged pixels to be filtered
;
num=n_elements(d1)
flag=fltarr(num)
ccd=d1.ccd_id
chipx=d1.chipx
dummy=0
print,' '
print,'    CCD_ID','     CHIPX'
print,'    ------','     -----'

;
; Set up plot environment
;
!p.multi=[0,2,3,0]
!fancy=6
open,'/portrait',plotfile


;
; Loop to locate bad columns on each chip
;
for i=0,nchips-1 do begin

;
;   Make histogram of CHIPX for this chip
;
    j=where(d1.ccd_id eq chips(i))
    plothist,d1(j).chipx,x,y,/noplot
    y=float(y)/n_elements(y)
    

;
;   Convert cnts/pixel into SNR
;
    moment,y,ave,med,sig
    snr=(y-med)/sig

;
;   Locate all columns which are above the cutoff
;
    k=where(snr gt cutoff(i))

    if (k(0) eq -1) then begin
       print,chips(i),'      NONE'
       printf,unit,' '
       printf,unit,chips(i),'      NONE'
       cluephone=0

    endif else begin
       cluephone=1
       xb=x(k)
       print,chips(i),xb
       printf,unit,' '
       printf,unit,chips(i),xb(0)
		for kz=1,n_elements(xb)-1 do begin
       			printf,unit,'        ',xb(kz)
		endfor

;
;      Set flags for those events on this chip which fall
;      in the indicated bad columns
;
       nbad=n_elements(xb)
       for ib=0,nbad-1 do begin
           il=where( (ccd eq chips(i)) and (chipx eq xb(ib)) )
           flag(il)=1.0
       endfor
    endelse    


;
;   Plot this chip
;
    plot,x,snr,psym=10,xtitle='!6CHIP X',xrange=[0,1024],/xst, $
               ytitle='SNR',title='CHIP '+strtrim(string(chips(i)),2)
    oplot,[0,1024],[cutoff(i),cutoff(i)]


;
; Create structure containing node boundaries
;

    if (chips(i) eq id1) then begin

	if (cluephone gt 0) then begin

		node=replicate(b1(0),nbad)
		node.shape='rectangle       '
		node.chipy=[1,1024]
		node.time=0.0000000
		node.status=[0,1]

		dat=size(b1)
		ns=n_elements(dat)

		if (dat(ns-2) eq 8) then begin
   			numb=n_elements(b1)
		endif else begin
			numb=0
		endelse	

		for j=0,nbad-1 do begin
   			node(j).component=numb+j+1
			node(j).chipx=[xb(j),0]
		endfor
		
		if (dat(ns-2) eq 8) then begin
   			f1=[b1,node]
		endif else begin
			f1=node
		endelse

   		sxaddpar,h1,'NAXIS2',numb+nbad
		sxaddpar,h1,'HISTORY',' Hot columns added '+blank	

	
	endif else begin

		f1=b1
		sxaddpar,h1,'HISTORY',' Hot columns added '

	endelse
    endif


    if (chips(i) eq id2) then begin

	if (cluephone gt 0) then begin

		node=replicate(b1(0),nbad)
		node.shape='rectangle       '
		node.chipy=[1,1024]
		node.time=0.0000000
		node.status=[0,1]

		dat=size(b2)
		ns=n_elements(dat)

		if (dat(ns-2) eq 8) then begin
   			numb=n_elements(b2)
		endif else begin
			numb=0
		endelse	
	
		for j=0,nbad-1 do begin
   			node(j).component=numb+j+1
			node(j).chipx=[xb(j),0]
		endfor

		blank='                    '
		
		if (dat(ns-2) eq 8) then begin
   			f2=[b2,node]
		endif else begin
			f2=node
		endelse

   		sxaddpar,h2,'NAXIS2',numb+nbad
		sxaddpar,h2,'HISTORY',' Hot columns added '+blank	

	endif else begin

		f2=b2
		sxaddpar,h2,'HISTORY',' Hot columns added '

	endelse

    endif

    if (chips(i) eq id3) then begin

	if (cluephone gt 0) then begin

		node=replicate(b1(0),nbad)
		node.shape='rectangle       '
		node.chipy=[1,1024]
		node.time=0.0000000
		node.status=[0,1]

		dat=size(b3)
		ns=n_elements(dat)

		if (dat(ns-2) eq 8) then begin
   			numb=n_elements(b3)
		endif else begin
			numb=0
		endelse	
	
		for j=0,nbad-1 do begin
   			node(j).component=numb+j+1
			node(j).chipx=[xb(j),0]
		endfor

		blank='                    '
		
		if (dat(ns-2) eq 8) then begin
   			f3=[b3,node]
		endif else begin
			f3=node
		endelse

   		sxaddpar,h3,'NAXIS2',numb+nbad
		sxaddpar,h3,'HISTORY',' Hot columns added '+blank	

	endif else begin

		f3=b3
		sxaddpar,h3,'HISTORY',' Hot columns added '

	endelse

    endif


    if (chips(i) eq id4) then begin

	if (cluephone gt 0) then begin

		node=replicate(b1(0),nbad)
		node.shape='rectangle       '
		node.chipy=[1,1024]
		node.time=0.0000000
		node.status=[0,1]

		dat=size(b4)
		ns=n_elements(dat)

		if (dat(ns-2) eq 8) then begin
   			numb=n_elements(b4)
		endif else begin
			numb=0
		endelse	
	
		for j=0,nbad-1 do begin
   			node(j).component=numb+j+1
			node(j).chipx=[xb(j),0]
		endfor

		blank='                    '
		
		if (dat(ns-2) eq 8) then begin
   			f4=[b4,node]
		endif else begin
			f4=node
		endelse

   		sxaddpar,h4,'NAXIS2',numb+nbad
		sxaddpar,h4,'HISTORY',' Hot columns added '+blank	

	endif else begin

		f4=b4
		sxaddpar,h4,'HISTORY',' Hot columns added '

	endelse

    endif


    if (chips(i) eq id5) then begin

	if (cluephone gt 0) then begin

		node=replicate(b1(0),nbad)
		node.shape='rectangle       '
		node.chipy=[1,1024]
		node.time=0.0000000
		node.status=[0,1]

	   	dat=size(b5)
		ns=n_elements(dat)

		if (dat(ns-2) eq 8) then begin
   			numb=n_elements(b5)
		endif else begin
			numb=0
		endelse	
	
		for j=0,nbad-1 do begin
   			node(j).component=numb+j+1
			node(j).chipx=[xb(j),0]
		endfor

		blank='                    '

		if (dat(ns-2) eq 8) then begin
   			f5=[b5,node]
		endif else begin
			f5=node
		endelse
   		
		sxaddpar,h5,'NAXIS2',numb+nbad
		sxaddpar,h5,'HISTORY',' Hot columns added '+blank	

	endif else begin

		f5=b5
		sxaddpar,h5,'HISTORY',' Hot columns added '

	endelse

    endif


    if (chips(i) eq id6) then begin

	if (cluephone gt 0) then begin

		node=replicate(b1(0),nbad)
		node.shape='rectangle       '
		node.chipy=[1,1024]
		node.time=0.0000000
		node.status=[0,1]

	   	dat=size(b6)
		ns=n_elements(dat)

		if (dat(ns-2) eq 8) then begin
   			numb=n_elements(b6)
		endif else begin
			numb=0
		endelse	
	
		for j=0,nbad-1 do begin
   			node(j).component=numb+j+1
			node(j).chipx=[xb(j),0]
		endfor

		blank='                    '
		
		if (dat(ns-2) eq 8) then begin
   			f6=[b6,node]
		endif else begin
			f6=node
		endelse

   		sxaddpar,h6,'NAXIS2',numb+nbad
		sxaddpar,h6,'HISTORY',' Hot columns added '+blank	

	endif else begin

		f6=b6
		sxaddpar,h6,'HISTORY',' Hot columns added '

	endelse

    endif


;
; Next chip
;
endfor


;
; Finish up plot
;
shut
!p.multi(*)=0

;
; Close output text file
;
close,unit
free_lun,unit
print,' '

;
; Now filter out all the "bad" events
;
ig=where(flag eq 0.0)
c1=d1(ig)

;
; Append edited event list and GTI extensions to the new eventfile
;
mwrfits,c1,oevtfile,hd1
mwrfits,d2,oevtfile,hd2
mwrfits,d3,oevtfile,hd3
mwrfits,d4,oevtfile,hd4
mwrfits,d5,oevtfile,hd5
mwrfits,d6,oevtfile,hd6
mwrfits,d7,oevtfile,hd7

;
; Copy new BPIX extensions to the output file
;

mwrfits,f1,obpixfile,h1
mwrfits,f2,obpixfile,h2
mwrfits,f3,obpixfile,h3
mwrfits,f4,obpixfile,h4
mwrfits,f5,obpixfile,h5
mwrfits,f6,obpixfile,h6

;
; Update the CHECKSUM and DATASUM keywords 
;
cmdstring="fchecksum "+oevtfile+" update+"
spawn,cmdstring,result

cmdstring="fchecksum "+obpixfile+" update+"
spawn,cmdstring,result

;
; Return to IDL
;
return
end





























