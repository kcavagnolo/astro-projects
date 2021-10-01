pro create_source_region,inpfile,outfile=outfile,data=data,cutoff=cutoff,$
                         expand=expand,subtract=subtract
;-----------------------------------------------------------------------
;
; Name: CREATE_SOURCE_REGION
;
; Purpose: Reads in an Chandra source file from CELLDETECT and creates
;          a DS9 region file.
;          
;          
; Inputs: 
;         
;         
; Comments: 
;           
;           
; Revision history:
;       written by Michael Wise		07-01-00
;	added expansion factor		07-06-00
;-----------------------------------------------------------------------
;
; Check for correct number of parameters
;
np=n_params(0)
if ((np lt 1) or (np gt 3)) then begin
   print,string(7B),'CALLING SEQUENCE: ', $
   'create_source_region,inpfile,outfile=outfile,data=data,cutoff=cutoff', $
   '                     expand=expand,subtract=subtract'
   return   
endif

;
; Set default SNR to include
;
if (n_elements(cutoff) eq 0) then cutoff=3.0

;
; Set default expansion factor
;
if (n_elements(expand) eq 0) then expand=1.0

;
; Read in data file
;
data=mrdfits(inpfile,1,hd)


;
; Make sure this is an AOFF file
;
type=sxpar(hd,'EXTNAME','Parameter EXTNAME not found')
type=strtrim(type,2)
if (type ne 'SRCLIST') then begin
   print,string(7B),'ERROR: This file is not a SRCLIST file'
   return   
endif


;
; Get OBSID out of header
;
obsid=sxpar(hd,'OBS_ID','Parameter OBS_ID not found')

;
; Set default output file name
;
if (n_elements(outfile) eq 0) then begin
   outfile='srclst_obsid'+strtrim(string(obsid),2)+'.reg'
endif


;
; Extract the positions of the sources
;
x=data.x
y=data.y
r=data.detsize
snr=data.snr

;
; Find all sources above the cutof SNR
;
i=where(snr ge cutoff)
xs=x(i)
ys=y(i)
rs=r(i)
num=n_elements(xs)

;
; Create the output file
;
get_lun,unit
openw,unit,outfile
printf,unit,'physical'
if keyword_set(subtract) then printf,unit,'field()'
for i=0,num-1 do begin
    if keyword_set(subtract) then begin
       printf,unit,'-circle(',xs(i),',',ys(i),',',rs(i)*expand,')'
    endif else begin
       printf,unit,'circle(',xs(i),',',ys(i),',',rs(i)*expand,')'
    endelse
endfor
close,unit
free_lun,unit


;
; Return to IDL
;
return
end
