pro hist_calc,data,derr,width,type,flag,bins,counts,a,fit
;-----------------------------------------------------------------------
; Name: HIST_CALC
; Purpose: Calculates the histogram of the input data array 
; Inputs: 
;          data   -- input data array 
;          derr   -- errors in data
;          width  -- bin width in data units; defaults to 0.1 of range
;          type   -- normalization indicator; 
;                    (0) Don't normalize
;                    (1) Normalize by maxmimum
;                    (2) Normalize by total number
;          flag   -- if set then a gaussian is fit to the data
; Outputs:
;          bins   -- array of bins
;          counts -- number per bin
;          a      -- vector containing output from the fit
;          fit    -- the fit to the data
; Comments: 
; Revision history:
;       written by Michael Wise, 11-1-89
;-----------------------------------------------------------------------
;
; Check for correct number of parameters
;
np=n_params(0)
if ((np lt 2) or (np gt 9)) then begin
   print,string(7B),'CALLING SEQUENCE: ', $
   'hist_calc,data,derr,[width,type,flag,bins,counts,a,fit]'
   return   
endif
if (np lt 3) then begin
   range=abs(max(data)-min(data))
   width=0.1*range 
endif
if (np lt 4) then type=0
if (np lt 5) then flag=0
;
; Compute histogram of data
;
nd=n_elements(data)
dcopy=data  &  wcopy=width
halfbin=wcopy/2.
ulim=fix(max((dcopy+derr)/wcopy))+1 
dlim=fix(min((dcopy-derr)/wcopy))-1
nbins=ulim-dlim+1
x=findgen(nbins)+dlim
x=x*wcopy
h=findgen(nbins)
for i=0L,nbins-1 do begin
    xcur=x(i)
    x1=xcur-halfbin
    x2=xcur+halfbin
;
;   Figure out how much of each object is in this bin
;
    totbin=0.0
    for j=0L,nd-1 do begin
        x0=data(j)
        sig=derr(j)
        a=(x1-x0)/sig
        b=(x2-x0)/sig
        sum=gaussint(b)-gaussint(a)
        totbin=totbin+sum
    endfor
    h(i)=totbin
endfor
;
; Create two extra bins becase IDL is stupid about endpoints
;
bins=fltarr(nbins+2)               &  counts=fltarr(nbins+2)
bins(1)=x                          &  counts(1)=h
bins(0)=bins(1)-wcopy              &  counts(0)=0.  
bins(nbins+1)=bins(nbins)+wcopy    &  counts(nbins+1)=0.
;
; Do gaussian fit
;
fit=gaussfit(bins,counts,a)
;
; Do indicated normalization
;
if (type eq 1) then begin
   fit=fit/max(counts)
   counts=counts/max(counts)
endif
if (type eq 2) then begin
   fit=fit/float(n_elements(data))
   counts=counts/float(n_elements(data))
endif
;
; Return to IDL
;
return
end
