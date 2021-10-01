pro moment,data,ave,med,sdev,sdom,adev,var,skew,kurt
;-----------------------------------------------------------------------
; Name: MOMENT
; Purpose: Calculates the moments of the given data
; Inputs: 
;         ave  -- mean        
;         med  -- median
;         sdev -- standard deviation
;         sdom -- standard error in mean
;         adev -- average deviation
;         var  -- variance
;         skew -- skewness
;         kurt -- kurtosis
; Comments: 
;         If holding variables are not specified on the call line,
;         the results are printed to the current output device.
; Revision history:
;       written by Michael Wise, 4-23-90
;-----------------------------------------------------------------------
;
; Check for correct number of parameters
;
np=n_params(0)
if ((np lt 1) or (np gt 9)) then begin
   print,string(7B),'CALLING SEQUENCE: ', $
   'moment,data,[ave,med,sdev,sdom,adev,var,skew,kurt]'
   return   
endif
;
; Calculate stuff
;
num=n_elements(data)
if (num le 1) then begin
   print,string(7B),'ERROR: ', $
   'Data array contains only one element'
   return   
endif
ave=total(data)/num
copy=data(sort(data))
num2=num/2
if (2*num2 eq num) then med=0.5*(copy(num2)+copy(num2-1)) $
                   else med=copy(num2+1)
diff=data-ave
diff2=diff*diff
var=total(diff2)/(num-1)
sdev=sqrt(var)
sdom=sdev/sqrt(num)
adev=total(abs(diff))/num
skew=total((diff/sdev)^3)/num
kurt=(total((diff/sdev)^4)/num)-3.
;
; Print results is indicated
;
if (np le 2 ) then begin
   print,'Mean:               ',ave
   print,'Median:             ',med
   print,'Std. Deviation:     ',sdev
   print,'Std. Error of Mean: ',sdom
   print,'Average Deviation:  ',adev
   print,'Variance:           ',var
   print,'Skewness:           ',skew
   print,'Kurtosis:           ',kurt
endif
;
; Return to IDL
;
return
end
