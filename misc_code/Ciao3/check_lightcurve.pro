pro check_lightcurve,inpfile,outfile=outfile,data=data,cutoff=cutoff
;-----------------------------------------------------------------------
;
; Name: CHECK_LIGHTCURVE
;
; Purpose: Reads in a FITS file containing the lightcurve and makes 
;          a diagnostic plot.
;          
;          
; Inputs: 
;         
;         
; Comments: 
;           
;           
; Revision history:
;       written by Michael Wise, 6-26-00
;-----------------------------------------------------------------------
;
; Check for correct number of parameters
;
np=n_params(0)
if ((np lt 1) or (np gt 4)) then begin
   print,string(7B),'CALLING SEQUENCE: ', $
   'check_lightcurve,inpfile,[outfile=outfile,data=data,cutoff=cutoff]'
   return   
endif


;
; Read in data file
;
data=mrdfits(inpfile,1,hd)


;
; Make sure this is an AOFF file
;
type=sxpar(hd,'EXTNAME','Parameter EXTNAME not found')
type=strtrim(type,2)
if (type ne 'LIGHTCURVE') then begin
   print,string(7B),'ERROR: This file is not a LIGHTCURVE file'
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
   outfile='lightcurve_obsid'+strtrim(string(obsid),2)+'.ps'
endif

;
; Set default cutoff value
;
if (n_elements(cutoff) eq 0) then cutoff=[0.8,1.2]


;
; Extract the plot variables
;
t=data.time-min(data.time)
t=t/1000.				; Convert to ksecs
r=data.rate				; Cnts/sec


;
; Calculate the quiescent rate
;
result=moment(r)
ave=result[0]
sig=sqrt(result[1])
med=median(r)
;
; Find all points which are greater than the cutoff
;
ib=where( (r lt cutoff(0)*med) or (r gt cutoff(1)*med) )
ig=where( (r ge cutoff(0)*med) and (r le cutoff(1)*med) )

;
; Recalculate quiescent rate
;
result=moment(r(ig))
ave=result[0]
sig=sqrt(result[1])
med=median(r(ig))
;
; Re-find all points which are greater than the cutoff
;
ib=where( (r lt cutoff(0)*med) or (r gt cutoff(1)*med) )
ig=where( (r ge cutoff(0)*med) and (r le cutoff(1)*med) )


;
; Set up the plot environment and plot limits
;
!p.multi=[0,0,2,0]
xmin=float(fix(min(t)))
xmax=float(fix(max(t))+1)
ymin=float(fix(min(r)))
ymax=float(fix(max(r))+1)

ygmin=med-5.0*sig
ygmax=med+5.0*sig
xc=xmin+0.1*(xmax-xmin)
yc=med+3.5*sig
info0='<R>= '+strtrim(string(med),2)+' +/- '+strtrim(string(sig),2)
info1=strtrim(string(med*cutoff(0)),2)
info2=strtrim(string(med*cutoff(1)),2)
info3='     ('+info1+','+info2+')'
info=info0+info3

;
; Make the plot
;
open,'/landscape',outfile

plot,t,r,psym=10,xtitle='!6Time [ksec]',ytitle='!6Rate [cnts/sec]',$
                 title='OBSID '+strtrim(string(obsid),2)+' Raw Lightcurve',$
                 xrange=[xmin,xmax],yrange=[ymin,ymax],/xst,/yst

plot,t(ig),r(ig),psym=10,xtitle='!6Time [ksec]',$
                 ytitle='!6Rate [cnts/sec]',$
                 title='Cleaned Lightcurve', $
                 xrange=[xmin,xmax],yrange=[ygmin,ygmax],/xst,/yst
oplot,[xmin,xmax],[med,med],line=2,thick=2
oplot,[xmin,xmax],[med,med]*cutoff(0),line=1,thick=2
oplot,[xmin,xmax],[med,med]*cutoff(1),line=1,thick=2
xyouts,xc,yc,info



shut

;
; Return to IDL
;
!p.multi(*)=0
return
end
