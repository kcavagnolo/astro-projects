pro make_cfn_file
;-----------------------------------------------------------------------
;
; Name: MAKE_CFN_FILE
;
; Purpose: Makes cooling function data file needed by the NEWCOOL program
;          
;          
; Inputs:  none
;         
;         
; Comments: 
;           
;           
; Revision history:
;       written by D&L, 2004-07-14
;-----------------------------------------------------------------------
;


;
; Open file
;
get_lun,unit
outfile='cfn.dat'
openw,unit,outfile


;
; Write data
;
printf,unit,'4.000    -23.00    -23.00'
printf,unit,'4.125    -21.85    -21.85'
printf,unit,'4.250    -21.53    -21.53'
printf,unit,'4.375    -21.82    -21.71'
printf,unit,'4.500    -22.12    -21.76'
printf,unit,'4.625    -22.39    -21.57'
printf,unit,'4.750    -22.32    -21.32'
printf,unit,'4.875    -21.95    -21.08'
printf,unit,'5.000    -22.01    -20.95'
printf,unit,'5.125    -22.31    -21.01'
printf,unit,'5.250    -22.58    -20.97'
printf,unit,'5.375    -22.78    -20.93'
printf,unit,'5.500    -22.97    -21.15'
printf,unit,'5.625    -23.12    -21.40'
printf,unit,'5.750    -23.22    -21.44'
printf,unit,'5.875    -23.29    -21.44'
printf,unit,'6.000    -23.33    -21.38'
printf,unit,'6.125    -23.34    -21.50'
printf,unit,'6.250    -23.34    -21.79'
printf,unit,'6.375    -23.32    -22.26'
printf,unit,'6.500    -23.28    -22.51'
printf,unit,'6.625    -23.25    -22.61'
printf,unit,'6.750    -23.21    -22.68'
printf,unit,'6.875    -23.16    -22.65'
printf,unit,'7.000    -23.10    -22.58'
printf,unit,'7.125    -23.04    -22.60'
printf,unit,'7.250    -22.99    -22.66'
printf,unit,'7.375    -22.93    -22.68'
printf,unit,'7.500    -22.87    -22.67'
printf,unit,'7.625    -22.81    -22.63'
printf,unit,'7.750    -22.75    -22.59'
printf,unit,'7.875    -22.69    -22.54'
printf,unit,'8.000    -22.64    -22.50'
printf,unit,' '


;
; Close file
;
close,unit
free_lun,unit


return
end