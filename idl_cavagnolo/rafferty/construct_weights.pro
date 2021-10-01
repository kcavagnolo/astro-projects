pro construct_weights,spectfile,rootname=rootname,bands=bands,files=files
;-----------------------------------------------------------------------
; Name: CONSTRUCT_WEIGHTS
;
; Purpose: Construct spectral weighting files from an XSPEC model file
;          
; Inputs: 
;         
; Comments: 
;           
; Revision history:
;       written by Michael Wise			11-13-99
;
;	added normalization to get		01-09-00
;	flux correct
;
;	modified model input format		08-25-00
;
;	fixed bug in band normalization		09-25-00
;
;-----------------------------------------------------------------------
;
; Check for correct number of parameters
;
np=n_params(0)
if ((np lt 1) or (np gt 20)) then begin
   print,string(7B),'CALLING SEQUENCE: ', $
   'construct_weights,spectfile,rootname=rootname,bands=bands,files=files'
   return   
endif

;
; Handle defaults
;
if (n_elements(rootname) eq 0) then rootname='spect_weights'
if (n_elements(bands) eq 0) then begin
   bands=fltarr(2,5)
   bands(0,*)=[0.5,1.5,3.0,5.0]		; Lower boundaries in keV
   bands(1,*)=[1.5,3.0,5.0,9.0]		; Upper boundaries in keV
endif


;
; Handle special case of one energy band
;
s=size(bands)
if (s(0) eq 1) then begin
   bflag=1
   nband=1
endif else begin
   bflag=0
   nband=s(2)
endelse


;
; Read in XSPEC model file
;
readcol,spectfile,x,z,y


;
; Normalize spectrum
;
tot=total(y)
y=y/tot


;
; Get file unit
;
get_lun,unit

;
; Loop over number of energy bands
;
files=strarr(nband)
for i=0L,nband-1 do begin
;
;   Set limits for this extraction
;
    if (bflag ne 1) then begin
       e1=bands(0,i)			; extract in keV like XSPEC
       e2=bands(1,i)			; extract in keV like XSPEC
    endif else begin
       e1=bands(0)			; extract in keV like XSPEC
       e2=bands(1)			; extract in keV like XSPEC
    endelse

;
;   Extract piece of spectrum in this band
;
    j=where( (x gt e1) and (x le e2) )
    if (j(0) eq -1) then begin
       print,string(7B),'ERROR: No points available for the band',e1,$
                        'to ',e2,' keV'
       return
    endif
    numw=n_elements(j)

;
;   Calculate normalization factor for this band
;
    tot_band=total(y(j))

;
;   Write out this file
;
    e1=fix(e1*1000.)		; name in eV
    e2=fix(e2*1000.)		; name in eV
    e1=strtrim(string(e1),2)
    e2=strtrim(string(e2),2)
    curfile=rootname+'_'+e1+'_'+e2+'.dat'
    files(i)=curfile
    openw,unit,curfile
    for k=0,numw-1 do begin
        printf,unit,x(j(k)),y(j(k))/tot_band
    endfor
    close,unit

;
; Next band
;
endfor

;
; Free file unit
;
free_lun,unit

;
; Return to IDL
;
return
end
