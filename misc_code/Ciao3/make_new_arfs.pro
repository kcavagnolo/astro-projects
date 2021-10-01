pro make_new_arfs,nreg,rootstr=rootstr,ciao3=ciao3,wide=wide,new_name_conv=new_name_conv
;-----------------------------------------------------------------------
;
; Name: MAKE_NEW_ARFS
;
; Purpose: Makes new weighted arfs with the CIAO 3.1 QE correction applied
;                  
; Inputs:  nreg - number of regions
;                
; Comments: 
;                     
; Revision history:
;       written by DAR, 2004-08-18
;-----------------------------------------------------------------------
;
; Check for correct number of parameters
;
np=n_params(0)
if ((np lt 1) or (np gt 1)) then begin
   print,string(7B),'CALLING SEQUENCE: ', $
   'make_new_arfs, nreg'
   return   
endif


;
; Define default root directory for CIAO
;
cmdroot='source /iraf/ciao_3.1/bin/ciao.csh ; '
if ( (keyword_set(new_name_conv)) and (n_elements(rootstr) eq 0) ) then rootstr='reg'
if (n_elements(rootstr) eq 0) then rootstr='deproj_reg'


;
; Create a weighted ARF
;
cd,'spectra'
for i=1,nreg do begin
   n=fix(i)
   root=rootstr+strtrim(string(n),2)
   if keyword_set(new_name_conv) then root=rootstr+strtrim(string(n),2)+'_sou'
   pi_file=root+'.pi'
   print,'Now creating a weighted ARF for '+root+'...'
   cmdstring='punlearn mkwarf'
   warf_file=root+'.warf'
   fef_weights=root+'.wgt'
   spawn,cmdroot+cmdstring,result

   pi_filter='"'+pi_file+'[wmap]"'
   if keyword_set(ciao3) then pi_filter=root+'.wmap'
;   cmdstring='rm '+warf_file
;   spawn,cmdstring,result
;   cmdstring='rm '+fef_weights
;   spawn,cmdstring,result
   if keyword_set(wide) then cmdstring='mkwarf infile='+pi_filter+' outfile='+warf_file+' weightfile='+fef_weights+' spectrumfile=NONE egridspec=0.3:11.0:0.01 clobber=yes' else cmdstring='mkwarf infile='+pi_filter+' outfile='+warf_file+' weightfile='+fef_weights+' spectrumfile=NONE egridspec=0.5:8.0:0.01 clobber=yes'
   spawn,cmdroot+cmdstring,result
   print,'...done.'
endfor
cd,'..'


;
; Return to IDL
;
return
end


